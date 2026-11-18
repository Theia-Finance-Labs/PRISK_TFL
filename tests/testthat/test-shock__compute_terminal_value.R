# Tests for compute_terminal_value()

testthat::test_that("compute_terminal_value uses the full endpoint AAL, not an interpolated rate", {
  trisk_joined <- tibble::tibble(
    asset = "A1", company = "C1", asset_category = "industrial building",
    year = 2050, net_profits_baseline_scenario = 1000, net_profits_shock_scenario = 1000
  )
  aal_endpoints <- list(
    rcp85 = tibble::tibble(asset = "A1", aal_pd_profit = 50, aal_pd_revenue_rate = 0, aal_bd_revenue_rate = 0.1),
    rcp26 = tibble::tibble(asset = "A1", aal_pd_profit = 20, aal_pd_revenue_rate = 0, aal_bd_revenue_rate = 0.05)
  )
  companies <- tibble::tibble(company = "C1", net_profit_margin = 0.1)

  result <- compute_terminal_value(
    trisk_joined = trisk_joined, aal_endpoints = aal_endpoints, companies = companies,
    growth_rate = 0.02, discount_rate = 0.05, terminal_year = 2050, anchor_year = 2026
  )

  # Baseline terminal profit uses the FULL rcp85 endpoint:
  # revenue = 1000/0.1 = 10000; revenue_adj = 10000*(1-0.1) = 9000;
  # profit_adj = 9000*0.1 = 900; terminal_profit = 900 - 50 = 850
  terminal_profit_baseline <- 850
  tv_nominal_baseline <- terminal_profit_baseline * (1 + 0.02) / (0.05 - 0.02)
  tv_present_baseline <- tv_nominal_baseline / (1 + 0.05)^(2050 - 2026)

  baseline_row <- result[result$scenario == "trisk_prisk_baseline", ]
  testthat::expect_equal(baseline_row$tv_nominal, tv_nominal_baseline, tolerance = 1e-6)
  testthat::expect_equal(baseline_row$tv_present_value, tv_present_baseline, tolerance = 1e-6)

  # Shock terminal profit uses the FULL rcp26 endpoint (a different, smaller AAL):
  # revenue_adj = 10000*(1-0.05) = 9500; profit_adj = 950; terminal_profit = 950-20 = 930
  terminal_profit_shock <- 930
  shock_row <- result[result$scenario == "trisk_prisk_shock", ]
  tv_nominal_shock <- terminal_profit_shock * (1 + 0.02) / (0.05 - 0.02)
  testthat::expect_equal(shock_row$tv_nominal, tv_nominal_shock, tolerance = 1e-6)

  # TRISK-only pathways get a terminal value from their raw, unadjusted profit
  # (no AAL at all) -- trisk_baseline and trisk_shock both use profit=1000 here.
  trisk_baseline_row <- result[result$scenario == "trisk_baseline", ]
  tv_nominal_trisk <- 1000 * (1 + 0.02) / (0.05 - 0.02)
  testthat::expect_equal(trisk_baseline_row$tv_nominal, tv_nominal_trisk, tolerance = 1e-6)

  # All four scenarios present
  testthat::expect_setequal(
    result$scenario,
    c("trisk_baseline", "trisk_shock", "trisk_prisk_baseline", "trisk_prisk_shock")
  )
})

testthat::test_that("compute_terminal_value errors when growth_rate >= discount_rate", {
  trisk_joined <- tibble::tibble(
    asset = "A1", company = "C1", asset_category = "industrial building",
    year = 2050, net_profits_baseline_scenario = 1000, net_profits_shock_scenario = 1000
  )
  aal_endpoints <- list(
    rcp85 = tibble::tibble(asset = "A1", aal_pd_profit = 0, aal_pd_revenue_rate = 0, aal_bd_revenue_rate = 0),
    rcp26 = tibble::tibble(asset = "A1", aal_pd_profit = 0, aal_pd_revenue_rate = 0, aal_bd_revenue_rate = 0)
  )
  companies <- tibble::tibble(company = "C1", net_profit_margin = 0.1)

  testthat::expect_error(
    compute_terminal_value(
      trisk_joined = trisk_joined, aal_endpoints = aal_endpoints, companies = companies,
      growth_rate = 0.05, discount_rate = 0.05, terminal_year = 2050, anchor_year = 2026
    ),
    "growth_rate must be less than discount_rate"
  )
})

testthat::test_that("compute_terminal_value errors when no rows exist for terminal_year", {
  trisk_joined <- tibble::tibble(
    asset = "A1", company = "C1", asset_category = "industrial building",
    year = 2040, net_profits_baseline_scenario = 1000, net_profits_shock_scenario = 1000
  )
  aal_endpoints <- list(
    rcp85 = tibble::tibble(asset = "A1", aal_pd_profit = 0, aal_pd_revenue_rate = 0, aal_bd_revenue_rate = 0),
    rcp26 = tibble::tibble(asset = "A1", aal_pd_profit = 0, aal_pd_revenue_rate = 0, aal_bd_revenue_rate = 0)
  )
  companies <- tibble::tibble(company = "C1", net_profit_margin = 0.1)

  testthat::expect_error(
    compute_terminal_value(
      trisk_joined = trisk_joined, aal_endpoints = aal_endpoints, companies = companies,
      growth_rate = 0.02, discount_rate = 0.05, terminal_year = 2050, anchor_year = 2026
    ),
    "no TRISK rows found"
  )
})

testthat::test_that("pivot_trisk_prisk_results produces one row per company with all 12 metric columns", {
  companies_el <- tibble::tibble(
    company = rep(c("C1", "C2"), each = 4),
    scenario = rep(c("trisk_baseline", "trisk_shock", "trisk_prisk_baseline", "trisk_prisk_shock"), 2),
    npv = seq(100, 800, by = 100),
    merton_pd = seq(0.01, 0.08, by = 0.01),
    expected_loss = seq(10, 80, by = 10)
  )

  result <- pivot_trisk_prisk_results(companies_el)

  testthat::expect_equal(nrow(result), 2)
  expected_cols <- c(
    "company",
    "npv_trisk_baseline", "npv_trisk_shock", "npv_trisk_prisk_baseline", "npv_trisk_prisk_shock",
    "pd_trisk_baseline", "pd_trisk_shock", "pd_trisk_prisk_baseline", "pd_trisk_prisk_shock",
    "expected_loss_trisk_baseline", "expected_loss_trisk_shock",
    "expected_loss_trisk_prisk_baseline", "expected_loss_trisk_prisk_shock"
  )
  testthat::expect_true(all(expected_cols %in% names(result)))

  c1 <- result[result$company == "C1", ]
  testthat::expect_equal(c1$npv_trisk_baseline, 100)
  testthat::expect_equal(c1$npv_trisk_prisk_shock, 400)
})
