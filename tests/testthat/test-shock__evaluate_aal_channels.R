# Tests for compute_aal_channels

flood_hazard_config <- list(
  shocks = list(
    revenue = list(
      equations = list(
        flood_non_agriculture = list(
          when = "asset_category != 'agriculture'",
          equation = "revenue ~ revenue * (1 - business_disruption / 365)"
        ),
        flood_agriculture = list(
          when = "asset_category == 'agriculture'",
          equation = "revenue ~ revenue * (1 - damage_factor) * (1 - business_disruption / 365)"
        )
      )
    ),
    profit = list(
      equations = list(
        flood_buildings = list(
          when = "asset_category %in% c('commercial building', 'industrial building')",
          equation = "profit ~ profit - (damage_factor * cost_factor)"
        )
      )
    )
  )
)

testthat::test_that("compute_aal_channels computes hand-computed AAL per channel per asset", {
  # A1: industrial building, cost_factor = 1000
  #   RP=10:  damage_factor=0.1, business_disruption=36.5 (BD rate = 0.1)
  #   RP=100: damage_factor=0.3, business_disruption=73.0 (BD rate = 0.2)
  # A2: agriculture (no cost_factor)
  #   RP=10:  damage_factor=0.05, business_disruption=18.25 (BD rate = 0.05)
  #   RP=100: damage_factor=0.20, business_disruption=36.5  (BD rate = 0.10)
  # A3: commercial building, cost_factor = 500
  #   RP=10:  damage_factor=0.2, business_disruption=36.5 (BD rate = 0.1)
  #   RP=100: damage_factor=0.4, business_disruption=73.0 (BD rate = 0.2)
  assets_factors_by_rp <- tibble::tibble(
    asset = c("A1", "A1", "A2", "A2", "A3", "A3"),
    return_period = c(10, 100, 10, 100, 10, 100),
    asset_category = c(
      "industrial building", "industrial building",
      "agriculture", "agriculture",
      "commercial building", "commercial building"
    ),
    damage_factor = c(0.1, 0.3, 0.05, 0.2, 0.2, 0.4),
    cost_factor = c(1000, 1000, NA, NA, 500, 500),
    business_disruption = c(36.5, 73.0, 18.25, 36.5, 36.5, 73.0)
  )

  result <- compute_aal_channels(assets_factors_by_rp, flood_hazard_config)

  testthat::expect_named(result, c("summary", "detail"))
  summary <- result$summary
  testthat::expect_equal(nrow(summary), 3)

  # Hand-computed via calculate_aal()'s documented trapezoidal method
  # (boundary points: RP=1/loss=0, and flat cap at AEP=0):
  # A1 pd_profit: rp=[10,100], loss=[100,300] -> AAL = 45 + 18 + 3 = 66
  # A1 bd_revenue_rate: rp=[10,100], loss=[0.1,0.2] -> AAL = 0.045+0.0135+0.002 = 0.0605
  # A1 pd_revenue_rate: 0 (not agriculture)
  a1 <- dplyr::filter(summary, asset == "A1")
  testthat::expect_equal(a1$aal_pd_profit, 66, tolerance = 1e-6)
  testthat::expect_equal(a1$aal_bd_revenue_rate, 0.0605, tolerance = 1e-6)
  testthat::expect_equal(a1$aal_pd_revenue_rate, 0, tolerance = 1e-6)

  # A2 (agriculture) pd_profit: 0 (buildings-only equation never matches agriculture)
  # A2 bd_revenue_rate: rp=[10,100], loss=[0.05,0.1] -> AAL = 0.0225+0.00675+0.001 = 0.03025
  # A2 pd_revenue_rate: rp=[10,100], loss=[0.05,0.2] -> AAL = 0.0225+0.01125+0.002 = 0.03575
  a2 <- dplyr::filter(summary, asset == "A2")
  testthat::expect_equal(a2$aal_pd_profit, 0, tolerance = 1e-6)
  testthat::expect_equal(a2$aal_bd_revenue_rate, 0.03025, tolerance = 1e-6)
  testthat::expect_equal(a2$aal_pd_revenue_rate, 0.03575, tolerance = 1e-6)

  # A3 (commercial building) pd_profit: rp=[10,100], loss=[100,200] -> AAL = 45+13.5+2 = 60.5
  # A3 bd_revenue_rate: same rates as A1 -> AAL = 0.0605
  # A3 pd_revenue_rate: 0 (not agriculture)
  a3 <- dplyr::filter(summary, asset == "A3")
  testthat::expect_equal(a3$aal_pd_profit, 60.5, tolerance = 1e-6)
  testthat::expect_equal(a3$aal_bd_revenue_rate, 0.0605, tolerance = 1e-6)
  testthat::expect_equal(a3$aal_pd_revenue_rate, 0, tolerance = 1e-6)

  # detail table carries the raw per-RP loss series behind the integration
  detail <- result$detail
  testthat::expect_true(all(c("asset", "return_period", "channel", "loss", "aep") %in% names(detail)))
  a1_pd_detail <- dplyr::filter(detail, asset == "A1", channel == "aal_pd_profit") |>
    dplyr::arrange(return_period)
  testthat::expect_equal(a1_pd_detail$loss, c(100, 300), tolerance = 1e-6)
  testthat::expect_equal(a1_pd_detail$aep, c(0.1, 0.01), tolerance = 1e-6)
})

testthat::test_that("compute_aal_channels returns empty summary/detail for empty input", {
  result <- compute_aal_channels(tibble::tibble(), flood_hazard_config)
  testthat::expect_equal(nrow(result$summary), 0)
  testthat::expect_equal(nrow(result$detail), 0)
  testthat::expect_named(result$summary, c("asset", "aal_pd_profit", "aal_pd_revenue_rate", "aal_bd_revenue_rate"))
})

testthat::test_that("compute_aal_channels treats an asset with no usable coverage as zero AAL, not a hard failure", {
  # A4 has NA damage_factor/business_disruption at every return period (e.g. a
  # precomputed-lookup asset whose region has no hazard coverage) -- this must
  # not halt the whole portfolio's computation.
  assets_factors_by_rp <- tibble::tibble(
    asset = c("A1", "A1", "A4", "A4"),
    return_period = c(10, 100, 10, 100),
    asset_category = c("industrial building", "industrial building", "industrial building", "industrial building"),
    damage_factor = c(0.1, 0.3, NA_real_, NA_real_),
    cost_factor = c(1000, 1000, 1000, 1000),
    business_disruption = c(36.5, 73.0, NA_real_, NA_real_)
  )

  result <- compute_aal_channels(assets_factors_by_rp, flood_hazard_config)

  a1 <- dplyr::filter(result$summary, asset == "A1")
  a4 <- dplyr::filter(result$summary, asset == "A4")
  testthat::expect_equal(a1$aal_pd_profit, 66, tolerance = 1e-6)
  testthat::expect_equal(a4$aal_pd_profit, 0)
  testthat::expect_equal(a4$aal_bd_revenue_rate, 0)
})

testthat::test_that("compute_aal_channels errors on missing required columns", {
  bad_input <- tibble::tibble(asset = "A1", return_period = 10)
  testthat::expect_error(
    compute_aal_channels(bad_input, flood_hazard_config),
    "missing required column"
  )
})
