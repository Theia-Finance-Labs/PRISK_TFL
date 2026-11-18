# Tests for apply_trisk_prisk_adjustment()

testthat::test_that("apply_trisk_prisk_adjustment applies the building channel split correctly", {
  # Building: profit=1000, npm=0.1 -> revenue=10000
  # revenue_adjusted = 10000 * (1 - 0.05) = 9500
  # profit_adjusted = 9500 * 0.1 = 950
  # profit_final = 950 - 20 (flat pd_profit deduction) = 930
  trisk_joined <- tibble::tibble(
    asset = "A1", company = "C1", asset_category = "industrial building",
    year = 2026,
    net_profits_baseline_scenario = 1000, net_profits_shock_scenario = 1000
  )
  aal_by_year <- tibble::tibble(
    asset = "A1", year = 2026, pathway = c("baseline", "shock"),
    aal_pd_profit = 20, aal_pd_revenue_rate = 0, aal_bd_revenue_rate = 0.05
  )
  companies <- tibble::tibble(company = "C1", net_profit_margin = 0.1)

  result <- apply_trisk_prisk_adjustment(trisk_joined, aal_by_year, companies)

  prisk_baseline <- result[result$scenario == "trisk_prisk_baseline", ]
  testthat::expect_equal(prisk_baseline$profit, 930)

  trisk_baseline <- result[result$scenario == "trisk_baseline", ]
  testthat::expect_equal(trisk_baseline$profit, 1000)
})

testthat::test_that("apply_trisk_prisk_adjustment applies the agriculture channel split correctly (no profit-side deduction)", {
  # Agriculture: profit=500, npm=0.2 -> revenue=2500
  # revenue_adjusted = 2500 * (1 - 0.03 - 0.04) = 2325
  # profit_final = 2325 * 0.2 = 465 -- no separate aal_pd_profit deduction
  trisk_joined <- tibble::tibble(
    asset = "A2", company = "C2", asset_category = "agriculture",
    year = 2026,
    net_profits_baseline_scenario = 500, net_profits_shock_scenario = 500
  )
  aal_by_year <- tibble::tibble(
    asset = "A2", year = 2026, pathway = c("baseline", "shock"),
    aal_pd_profit = 999, aal_pd_revenue_rate = 0.04, aal_bd_revenue_rate = 0.03
  )
  companies <- tibble::tibble(company = "C2", net_profit_margin = 0.2)

  result <- apply_trisk_prisk_adjustment(trisk_joined, aal_by_year, companies)

  prisk_baseline <- result[result$scenario == "trisk_prisk_baseline", ]
  testthat::expect_equal(prisk_baseline$profit, 465)
})

testthat::test_that("apply_trisk_prisk_adjustment skips adjustment for non-positive TRISK profit, and floors the (unadjusted) result at 0", {
  trisk_joined <- tibble::tibble(
    asset = c("A3", "A4"), company = c("C1", "C1"),
    asset_category = c("industrial building", "commercial building"),
    year = c(2030, 2030),
    net_profits_baseline_scenario = c(0, 500),
    net_profits_shock_scenario = c(-250, -10)
  )
  aal_by_year <- tibble::tibble(
    asset = rep(c("A3", "A4"), each = 2), year = 2030,
    pathway = rep(c("baseline", "shock"), 2),
    aal_pd_profit = 50, aal_pd_revenue_rate = 0, aal_bd_revenue_rate = 0.1
  )
  companies <- tibble::tibble(company = "C1", net_profit_margin = 0.1)

  result <- apply_trisk_prisk_adjustment(trisk_joined, aal_by_year, companies)
  prisk <- result[grepl("^trisk_prisk_", result$scenario), ]
  trisk_only <- result[!grepl("^trisk_prisk_", result$scenario), ]

  # A3 baseline = 0 -> early exit, no revenue reverse-engineered, profit stays 0
  a3_baseline <- prisk[prisk$asset == "A3" & prisk$scenario == "trisk_prisk_baseline", ]
  testthat::expect_equal(a3_baseline$profit, 0)
  testthat::expect_true(is.na(a3_baseline$revenue))

  # A3 shock = -250 -> early exit (no revenue/AAL applied), but profit is
  # floored at 0 in the output -- negative profit pathways are never returned.
  a3_shock <- prisk[prisk$asset == "A3" & prisk$scenario == "trisk_prisk_shock", ]
  testthat::expect_equal(a3_shock$profit, 0)
  testthat::expect_true(is.na(a3_shock$revenue))

  # The unadjusted trisk_shock pathway is floored the same way
  a3_shock_trisk_only <- trisk_only[trisk_only$asset == "A3" & trisk_only$scenario == "trisk_shock", ]
  testthat::expect_equal(a3_shock_trisk_only$profit, 0)

  # A4 baseline = 500 (positive) -> DOES get adjusted (sanity check the early
  # exit doesn't accidentally also skip positive-profit rows)
  a4_baseline <- prisk[prisk$asset == "A4" & prisk$scenario == "trisk_prisk_baseline", ]
  testthat::expect_false(is.na(a4_baseline$revenue))
  testthat::expect_true(a4_baseline$profit != 500)

  # A4 shock = -10 -> early exit, floored at 0 (not -10)
  a4_shock <- prisk[prisk$asset == "A4" & prisk$scenario == "trisk_prisk_shock", ]
  testthat::expect_equal(a4_shock$profit, 0)
})

testthat::test_that("apply_trisk_prisk_adjustment floors AAL-adjusted profit at 0 when the deduction would push it negative", {
  # Building: profit=100, npm=0.1 -> revenue=1000
  # revenue_adjusted = 1000 * (1 - 0.05) = 950; profit_adjusted = 95
  # profit_final = 95 - 200 (flat pd_profit deduction) = -105 -> floored to 0
  trisk_joined <- tibble::tibble(
    asset = "A5", company = "C1", asset_category = "industrial building",
    year = 2030, net_profits_baseline_scenario = 100, net_profits_shock_scenario = 100
  )
  aal_by_year <- tibble::tibble(
    asset = "A5", year = 2030, pathway = c("baseline", "shock"),
    aal_pd_profit = 200, aal_pd_revenue_rate = 0, aal_bd_revenue_rate = 0.05
  )
  companies <- tibble::tibble(company = "C1", net_profit_margin = 0.1)

  result <- apply_trisk_prisk_adjustment(trisk_joined, aal_by_year, companies)
  prisk_baseline <- result[result$scenario == "trisk_prisk_baseline", ]

  testthat::expect_equal(prisk_baseline$profit, 0)
  testthat::expect_true(all(result$profit >= 0))
})

testthat::test_that("apply_trisk_prisk_adjustment produces exactly the four expected scenario labels", {
  trisk_joined <- tibble::tibble(
    asset = "A1", company = "C1", asset_category = "industrial building",
    year = 2026, net_profits_baseline_scenario = 100, net_profits_shock_scenario = 100
  )
  aal_by_year <- tibble::tibble(
    asset = "A1", year = 2026, pathway = c("baseline", "shock"),
    aal_pd_profit = 0, aal_pd_revenue_rate = 0, aal_bd_revenue_rate = 0
  )
  companies <- tibble::tibble(company = "C1", net_profit_margin = 0.1)

  result <- apply_trisk_prisk_adjustment(trisk_joined, aal_by_year, companies)

  testthat::expect_setequal(
    unique(result$scenario),
    c("trisk_baseline", "trisk_shock", "trisk_prisk_baseline", "trisk_prisk_shock")
  )
  # zero AAL everywhere -> trisk_prisk pathways should equal the TRISK pathways
  testthat::expect_equal(
    result$profit[result$scenario == "trisk_prisk_baseline"],
    result$profit[result$scenario == "trisk_baseline"],
    tolerance = 1e-8
  )
})

testthat::test_that("apply_trisk_prisk_adjustment errors when a company has no net_profit_margin", {
  trisk_joined <- tibble::tibble(
    asset = "A1", company = "C_UNKNOWN", asset_category = "industrial building",
    year = 2026, net_profits_baseline_scenario = 100, net_profits_shock_scenario = 100
  )
  aal_by_year <- tibble::tibble(
    asset = "A1", year = 2026, pathway = c("baseline", "shock"),
    aal_pd_profit = 0, aal_pd_revenue_rate = 0, aal_bd_revenue_rate = 0
  )
  companies <- tibble::tibble(company = "C1", net_profit_margin = 0.1)

  testthat::expect_error(
    apply_trisk_prisk_adjustment(trisk_joined, aal_by_year, companies),
    "net_profit_margin"
  )
})

testthat::test_that("apply_trisk_prisk_adjustment errors on missing required columns", {
  testthat::expect_error(
    apply_trisk_prisk_adjustment(
      tibble::tibble(asset = "A1"),
      tibble::tibble(),
      tibble::tibble(company = "C1", net_profit_margin = 0.1)
    ),
    "missing required column"
  )
})
