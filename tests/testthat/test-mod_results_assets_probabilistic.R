# build_probabilistic_assets_table(): one panel per probabilistic event, even when a Flood and a
# Heat AAL event were run together (their per-return-period rows are bound into one table).

prob_flood_rows <- function() {
  tibble::tibble(
    asset = rep(c("A1", "A2"), each = 2), company = "C", asset_category = "commercial building",
    share_of_economic_activity = 1, hazard_type = "Flood", return_period = rep(c(5L, 100L), 2),
    flood_depth_cm = c(10, 50, 0, 20), damage_factor = c(0.1, 0.3, 0, 0.2), business_disruption = c(1, 5, 0, 2)
  )
}

prob_heat_rows <- function() {
  tibble::tibble(
    asset = rep(c("A1", "A2"), each = 3), company = "C", asset_category = "commercial building",
    share_of_economic_activity = 1, hazard_type = "Heat", return_period = rep(c(2, 10, 100), 2),
    excess_heat_days = c(10.123456, 20.987654, 30.5, 0, 0, 0), nace_code = rep(c("1.1", "10"), each = 3),
    lp_exposure = rep(c("High", "Medium"), each = 3), lp_loss = rep(c(0.208, 0.118), each = 3), statistic = "median"
  )
}

prob_aal <- function(with_heat = TRUE) {
  s <- tibble::tibble(
    asset = c("A1", "A2"), aal_pd_profit = c(1000, 0), aal_pd_revenue_rate = 0, aal_bd_revenue_rate = c(0.01, 0)
  )
  if (with_heat) s$aal_lp_revenue_rate <- c(0.0123456789, 0)
  s
}

test_that("a flood-only run keeps the original flood table untouched", {
  tbl <- build_probabilistic_assets_table(prob_flood_rows(), prob_aal(with_heat = FALSE))
  expect_equal(nrow(tbl), 2)
  expect_true(all(c(
    "Flood Depth (cm) (RP=5)", "Damage Factor (RP=100)", "Business Disruption (days) (RP=5)",
    "AAL Physical Damage - Profit ($)", "AAL Business Disruption - Revenue Rate"
  ) %in% names(tbl)))
  expect_false(any(grepl("Heat|Labour", names(tbl))))
  expect_equal(tbl[["AAL Physical Damage - Profit ($)"]], c("$1,000", "$0"))
})

test_that("with Flood and Heat bound together each event's panel shows only its own hazard", {
  both <- dplyr::bind_rows(prob_flood_rows(), prob_heat_rows())

  flood_tbl <- build_probabilistic_assets_table(both, prob_aal(), hazard_type_val = "Flood")
  # regression: the all-NA excess_heat_days column from the row-bind must NOT turn this into a heat table
  expect_false(any(grepl("Heat|Labour|lp_|nace", names(flood_tbl))))
  expect_true("Flood Depth (cm) (RP=5)" %in% names(flood_tbl))
  expect_true("AAL Physical Damage - Profit ($)" %in% names(flood_tbl))
  expect_equal(nrow(flood_tbl), 2)
  expect_equal(flood_tbl[["Flood Depth (cm) (RP=100)"]], c(50, 20))

  heat_tbl <- build_probabilistic_assets_table(both, prob_aal(), hazard_type_val = "Heat")
  expect_true(all(c(
    "Excess Heat Days (RP=2)", "Excess Heat Days (RP=10)", "Excess Heat Days (RP=100)",
    "nace_code", "lp_exposure", "lp_loss", "statistic"
  ) %in% names(heat_tbl)))
  expect_false(any(grepl("Flood|Damage|Disruption", names(heat_tbl))))
  expect_equal(nrow(heat_tbl), 2)
  # numbers are rounded for display
  expect_equal(heat_tbl[["Excess Heat Days (RP=2)"]], c(10.1, 0))
  aal_col <- "AAL Labour Productivity - Revenue Rate (mean over years)"
  expect_true(aal_col %in% names(heat_tbl))
  expect_equal(heat_tbl[[aal_col]], c(0.012346, 0))
})

test_that("a hazard type with no rows yields an empty table instead of an error", {
  expect_equal(nrow(build_probabilistic_assets_table(prob_flood_rows(), prob_aal(), hazard_type_val = "Heat")), 0)
  expect_equal(nrow(build_probabilistic_assets_table(NULL, prob_aal())), 0)
  expect_equal(nrow(build_probabilistic_assets_table(tibble::tibble(), prob_aal())), 0)
})

test_that("a heat-only run builds the heat table without a hazard type argument", {
  tbl <- build_probabilistic_assets_table(prob_heat_rows(), prob_aal())
  expect_true("Excess Heat Days (RP=2)" %in% names(tbl))
  expect_false(any(grepl("Damage", names(tbl))))
})

test_that("the heat table shows output change, IMF group and the average annual revenue adjustment (not s)", {
  heat <- prob_heat_rows() |>
    dplyr::mutate(
      country = rep(c("DE", "TH"), each = 3), imf_group = rep(c("Advanced Economies", "Emerging Market and Developing Economies"), each = 3),
      beta_L = rep(c(0.535, 0.399), each = 3), beta_L_source = "IMF_classification",
      s = c(0.0123456789, 0.02, 0.03, 0, 0, 0), output_change_pct = c(-0.0098765432, -0.01, -0.02, 0, 0, 0)
    )
  aal <- prob_aal() |> dplyr::mutate(avg_annual_revenue_adjustment = c(-12345.678, 0))
  tbl <- build_probabilistic_assets_table(heat, aal, hazard_type_val = "Heat")

  expect_true(all(c("country", "imf_group", "beta_L", "beta_L_source") %in% names(tbl)))
  expect_true("Output Change (fraction) (RP=100)" %in% names(tbl))
  # the labour shock s stays in the backend results but is not shown
  expect_false(any(grepl("Labour Shock", names(tbl))))
  expect_equal(tbl[["Output Change (fraction) (RP=2)"]], c(-0.009877, 0))
  expect_equal(tbl[["Average Annual Revenue Adjustment ($)"]], c(-12346, 0))
  # none of the removed Cobb-Douglas columns
  expect_false(any(grepl("^(E0|K0|L0)$|cb_", names(tbl))))
})

test_that("backend-only heat columns are hidden from tables but stay in the results", {
  heat <- tibble::tibble(
    asset = c("A1", "A2"), hazard_type = "Heat", excess_heat_days = c(10, 0), lp_exposure = "High",
    s = c(0.02, 0), baseline_revenue = c(1000, 500), revenue_adjustment = c(-7, 0), nace_level = c("1.1", "1"),
    nace_match = "exact", imf_country = "Germany", imf_match = "iso2", heat_map_weights = "2030.5:1",
    spatial_included = TRUE, output_change_pct = c(-0.007, 0), imf_group = "Advanced Economies"
  )
  shown <- drop_heat_backend_columns(heat)
  expect_false(any(c(
    "s", "baseline_revenue", "revenue_adjustment", "nace_level", "nace_match", "imf_country",
    "imf_match", "heat_map_weights", "spatial_included"
  ) %in% names(shown)))
  expect_true(all(c("asset", "excess_heat_days", "lp_exposure", "output_change_pct", "imf_group") %in% names(shown)))
  expect_equal(shown$excess_heat_days, heat$excess_heat_days)

  # a mixed heat + flood table (the full download) keeps spatial_included for the flood rows
  mixed <- dplyr::bind_rows(heat, tibble::tibble(asset = "F1", hazard_type = "Flood", spatial_included = TRUE))
  mixed_shown <- drop_heat_backend_columns(mixed)
  expect_true("spatial_included" %in% names(mixed_shown))
  expect_false("revenue_adjustment" %in% names(mixed_shown))

  # tables without heat columns (flood, Brazil) are returned untouched
  flood <- tibble::tibble(asset = "F1", hazard_type = "Flood", s = 1, spatial_included = TRUE)
  expect_identical(drop_heat_backend_columns(flood), flood)
  expect_null(drop_heat_backend_columns(NULL))
})
