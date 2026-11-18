# Tests for compute_aal_shock_trajectories

testthat::test_that("compute_aal_shock_trajectories deducts the AAL from every year, not just one", {
  yearly_baseline <- tibble::tibble(
    asset = c("A1", "A1", "A2", "A2"),
    company = c("C1", "C1", "C1", "C1"),
    year = c(2025, 2026, 2025, 2026),
    revenue = c(1000, 1100, 2000, 2200),
    profit = c(100, 110, 200, 220)
  )

  companies <- tibble::tibble(company = "C1", net_profit_margin = 0.1)

  aal_channels <- tibble::tibble(
    asset = c("A1", "A2"),
    aal_pd_profit = c(66, 0),
    aal_pd_revenue_rate = c(0, 0.03575),
    aal_bd_revenue_rate = c(0.0605, 0.03025)
  )

  result <- compute_aal_shock_trajectories(yearly_baseline, aal_channels, companies)

  testthat::expect_named(result, c("asset", "company", "year", "revenue", "profit"))
  testthat::expect_equal(nrow(result), 4)

  result_sorted <- result[order(result$asset, result$year), ]

  # A1 (buildings): combined revenue rate = 0 + 0.0605 = 0.0605
  #   revenue(2025) = 1000 * (1 - 0.0605) = 939.5;  profit = 93.95 - 66 = 27.95
  #   revenue(2026) = 1100 * (1 - 0.0605) = 1033.45; profit = 103.345 - 66 = 37.345
  a1 <- dplyr::filter(result_sorted, asset == "A1")
  testthat::expect_equal(a1$revenue, c(939.5, 1033.45), tolerance = 1e-8)
  testthat::expect_equal(a1$profit, c(27.95, 37.345), tolerance = 1e-8)

  # A2 (agriculture): combined revenue rate = 0.03575 + 0.03025 = 0.066, pd_profit = 0
  #   revenue(2025) = 2000 * (1 - 0.066) = 1868;   profit = 186.8 - 0 = 186.8
  #   revenue(2026) = 2200 * (1 - 0.066) = 2054.8;  profit = 205.48 - 0 = 205.48
  a2 <- dplyr::filter(result_sorted, asset == "A2")
  testthat::expect_equal(a2$revenue, c(1868, 2054.8), tolerance = 1e-8)
  testthat::expect_equal(a2$profit, c(186.8, 205.48), tolerance = 1e-8)

  # Every single year must differ from baseline (inverting the acute-shock test
  # pattern, which asserts only one event year differs) -- that's the entire
  # point of AAL vs. a single-event-year deterministic shock.
  testthat::expect_true(all(result_sorted$profit != yearly_baseline[order(yearly_baseline$asset, yearly_baseline$year), ]$profit))
})

testthat::test_that("compute_aal_shock_trajectories treats a missing asset in aal_channels as zero AAL", {
  yearly_baseline <- tibble::tibble(
    asset = "A3", company = "C1", year = c(2025, 2026),
    revenue = c(500, 550), profit = c(50, 55)
  )
  companies <- tibble::tibble(company = "C1", net_profit_margin = 0.1)
  # A3 not present in aal_channels at all
  aal_channels <- tibble::tibble(
    asset = "A1", aal_pd_profit = 10, aal_pd_revenue_rate = 0, aal_bd_revenue_rate = 0.05
  )

  result <- compute_aal_shock_trajectories(yearly_baseline, aal_channels, companies)
  # No AAL applies to A3 -> revenue unchanged, profit = revenue * NPM (recomputed
  # via step 2, matching compute_profits_from_revenue exactly)
  testthat::expect_equal(result$revenue, c(500, 550), tolerance = 1e-8)
  testthat::expect_equal(result$profit, c(50, 55), tolerance = 1e-8)
})

testthat::test_that("compute_aal_shock_trajectories returns baseline unchanged when aal_channels is empty", {
  yearly_baseline <- tibble::tibble(
    asset = "A1", company = "C1", year = 2025, revenue = 1000, profit = 100
  )
  companies <- tibble::tibble(company = "C1", net_profit_margin = 0.1)

  result <- compute_aal_shock_trajectories(yearly_baseline, tibble::tibble(), companies)
  testthat::expect_equal(result$revenue, 1000)
  testthat::expect_equal(result$profit, 100)
})

testthat::test_that("compute_aal_shock_trajectories errors on missing required baseline columns", {
  bad_baseline <- tibble::tibble(asset = "A1", year = 2025, revenue = 1000)
  companies <- tibble::tibble(company = "C1", net_profit_margin = 0.1)
  aal_channels <- tibble::tibble(
    asset = "A1", aal_pd_profit = 0, aal_pd_revenue_rate = 0, aal_bd_revenue_rate = 0
  )
  testthat::expect_error(
    compute_aal_shock_trajectories(bad_baseline, aal_channels, companies),
    "missing required column"
  )
})
