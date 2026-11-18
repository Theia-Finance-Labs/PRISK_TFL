# Tests for interpolate_aal_pathways() (the pure interpolation step behind
# compute_trisk_aal_by_year(), tested here without real hazard raster data)

testthat::test_that("interpolate_aal_pathways matches present exactly at the anchor year", {
  present <- tibble::tibble(
    asset = c("A1", "A2"),
    aal_pd_profit = c(1000, 0),
    aal_pd_revenue_rate = c(0, 0.01),
    aal_bd_revenue_rate = c(0.02, 0.03)
  )
  rcp85 <- tibble::tibble(
    asset = c("A1", "A2"),
    aal_pd_profit = c(2000, 0),
    aal_pd_revenue_rate = c(0, 0.05),
    aal_bd_revenue_rate = c(0.04, 0.07)
  )
  rcp26 <- tibble::tibble(
    asset = c("A1", "A2"),
    aal_pd_profit = c(1500, 0),
    aal_pd_revenue_rate = c(0, 0.03),
    aal_bd_revenue_rate = c(0.03, 0.05)
  )

  result <- interpolate_aal_pathways(
    present_summary = present, rcp26_summary = rcp26, rcp85_summary = rcp85,
    years = 2026:2028, anchor_year = 2026, endpoint_year = 2075
  )

  baseline_2026 <- result[result$year == 2026 & result$pathway == "baseline", ]
  baseline_2026 <- baseline_2026[order(baseline_2026$asset), ]
  testthat::expect_equal(baseline_2026$aal_pd_profit, present$aal_pd_profit)
  testthat::expect_equal(baseline_2026$aal_pd_revenue_rate, present$aal_pd_revenue_rate)
  testthat::expect_equal(baseline_2026$aal_bd_revenue_rate, present$aal_bd_revenue_rate)

  shock_2026 <- result[result$year == 2026 & result$pathway == "shock", ]
  shock_2026 <- shock_2026[order(shock_2026$asset), ]
  testthat::expect_equal(shock_2026$aal_pd_profit, present$aal_pd_profit)
})

testthat::test_that("interpolate_aal_pathways produces the correct partial-weight value mid-pathway", {
  present <- tibble::tibble(asset = "A1", aal_pd_profit = 1000, aal_pd_revenue_rate = 0, aal_bd_revenue_rate = 0.02)
  rcp85 <- tibble::tibble(asset = "A1", aal_pd_profit = 2000, aal_pd_revenue_rate = 0, aal_bd_revenue_rate = 0.04)
  rcp26 <- tibble::tibble(asset = "A1", aal_pd_profit = 1500, aal_pd_revenue_rate = 0, aal_bd_revenue_rate = 0.03)

  result <- interpolate_aal_pathways(
    present_summary = present, rcp26_summary = rcp26, rcp85_summary = rcp85,
    years = 2050, anchor_year = 2026, endpoint_year = 2075
  )

  # weight = (2050 - 2026) / (2075 - 2026) = 24/49
  weight <- 24 / 49
  baseline <- result[result$pathway == "baseline", ]
  shock <- result[result$pathway == "shock", ]

  # baseline (present -> rcp85): 1000 + weight * (2000 - 1000)
  testthat::expect_equal(baseline$aal_pd_profit, 1000 + weight * (2000 - 1000))
  testthat::expect_equal(baseline$aal_bd_revenue_rate, 0.02 + weight * (0.04 - 0.02))

  # shock (present -> rcp26): 1000 + weight * (1500 - 1000)
  testthat::expect_equal(shock$aal_pd_profit, 1000 + weight * (1500 - 1000))
  testthat::expect_equal(shock$aal_bd_revenue_rate, 0.02 + weight * (0.03 - 0.02))

  # The interpolated value must be strictly between present and the endpoint --
  # not equal to either -- since 2026 < 2050 < 2075.
  testthat::expect_true(baseline$aal_pd_profit > 1000 && baseline$aal_pd_profit < 2000)
})

testthat::test_that("interpolate_aal_pathways returns one row per asset per year per pathway", {
  present <- tibble::tibble(asset = c("A1", "A2", "A3"), aal_pd_profit = 0, aal_pd_revenue_rate = 0, aal_bd_revenue_rate = 0)
  endpoint <- present

  result <- interpolate_aal_pathways(
    present_summary = present, rcp26_summary = endpoint, rcp85_summary = endpoint,
    years = 2026:2030, anchor_year = 2026, endpoint_year = 2075
  )

  # 3 assets * 5 years * 2 pathways = 30
  testthat::expect_equal(nrow(result), 30)
  testthat::expect_setequal(unique(result$pathway), c("baseline", "shock"))
})

testthat::test_that("interpolate_aal_pathways: identical present/endpoint yields a flat pathway", {
  flat <- tibble::tibble(asset = "A1", aal_pd_profit = 500, aal_pd_revenue_rate = 0.01, aal_bd_revenue_rate = 0.02)

  result <- interpolate_aal_pathways(
    present_summary = flat, rcp26_summary = flat, rcp85_summary = flat,
    years = 2026:2050, anchor_year = 2026, endpoint_year = 2075
  )

  testthat::expect_true(all(result$aal_pd_profit == 500))
  testthat::expect_true(all(result$aal_pd_revenue_rate == 0.01))
  testthat::expect_true(all(result$aal_bd_revenue_rate == 0.02))
})
