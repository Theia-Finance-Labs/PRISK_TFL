# Tests for join_global_damage_cost_factors() (Global mode / TRISK-PRISK
# country + region-aware flood damage/cost factor joining) and the branch in
# join_damage_cost_factors() that routes to it.
#
# Fixtures: tests/testthat/fixtures/global_flood_mappings/hazards/mappings/
#   iso2_to_region.csv:            BR->Central & South America, US->North America,
#                                   DE->Europe, GB->Europe, "NA" (Namibia)->Africa
#   flood_damage_factors_global.csv: commercial building rows for Central & South
#     America / North America / Global (depths 0, 50, 100); agriculture rows only
#     for Global. No rows anywhere for "industrial building" (genuinely missing),
#     no rows for Europe/commercial or Central & South America/agriculture
#     (both covered only by the Global fallback).
#   flood_cost_factors_global.csv: wide, one row per country (Brazil, United
#     States, Germany, United Kingdom [Iso2 "UK", aliased to "GB"]).

test_global_hazard_config <- function() {
  list(
    Flood = list(
      indicators = list(
        flood_depth = list(variable = "flood_depth_cm", index = c("scenario_name", "return_period"))
      ),
      index_indicator = "flood_depth",
      primary_indicator = "flood_depth",
      mappings = list(
        iso2_region = list(file = "iso2_to_region.csv"),
        damage_factors_global = list(file = "flood_damage_factors_global.csv"),
        cost_factors_global = list(file = "flood_cost_factors_global.csv")
      )
    )
  )
}

test_global_hazards_dir <- function() {
  testthat::test_path("fixtures", "global_flood_mappings", "hazards", "config_global")
}

make_flood_row <- function(asset, asset_category, country, flood_depth_cm, event_id = asset) {
  tibble::tibble(
    asset = asset, company = "C1", event_id = event_id,
    hazard_type = "Flood", hazard_indicator = "flood_depth",
    asset_category = asset_category, country = country,
    flood_depth_cm = flood_depth_cm
  )
}

testthat::test_that("join_global_damage_cost_factors matches exact region/asset_category/depth and country cost factor", {
  assets <- make_flood_row("A1", "commercial building", "BR", 50)
  result <- join_global_damage_cost_factors(assets, test_global_hazard_config(), test_global_hazards_dir())

  row <- result[result$asset == "A1", ]
  testthat::expect_equal(row$region, "Central & South America")
  testthat::expect_equal(row$damage_factor, 0.05)
  testthat::expect_equal(row$business_disruption, 5)
  testthat::expect_equal(row$cost_factor, 100000)
})

testthat::test_that("join_global_damage_cost_factors snaps flood_depth_cm to the closest available grid value", {
  assets <- make_flood_row("A1", "commercial building", "BR", 60)
  result <- join_global_damage_cost_factors(assets, test_global_hazard_config(), test_global_hazards_dir())

  row <- result[result$asset == "A1", ]
  # 60 is closer to 50 than 100 in the 0/50/100 grid
  testthat::expect_equal(row$damage_factor, 0.05)
  testthat::expect_equal(row$business_disruption, 5)
})

testthat::test_that("join_global_damage_cost_factors falls back to the Global region when the asset's own region has no row for its asset_category", {
  # DE -> Europe, but the fixture has no Europe/commercial building rows
  assets <- make_flood_row("A1", "commercial building", "DE", 0)
  result <- suppressMessages(
    join_global_damage_cost_factors(assets, test_global_hazard_config(), test_global_hazards_dir())
  )

  row <- result[result$asset == "A1", ]
  testthat::expect_equal(row$region, "Europe")
  testthat::expect_equal(row$damage_factor, 0.015)   # Global commercial building at depth 0
  testthat::expect_equal(row$business_disruption, 1.5)
  testthat::expect_equal(row$cost_factor, 140000)    # Germany's own cost factor, unaffected by the region fallback
})

testthat::test_that("join_global_damage_cost_factors falls back to Global region for agriculture where the asset's region lacks it", {
  # BR -> Central & South America, but the fixture has no C&SA/agriculture rows
  assets <- make_flood_row("A1", "agriculture", "BR", 100)
  result <- suppressMessages(
    join_global_damage_cost_factors(assets, test_global_hazard_config(), test_global_hazards_dir())
  )

  row <- result[result$asset == "A1", ]
  testthat::expect_equal(row$damage_factor, 0.12)   # Global agriculture at depth 100
  testthat::expect_equal(row$business_disruption, 12)
  testthat::expect_true(is.na(row$cost_factor))     # agriculture has no cost_factor column in the source
})

testthat::test_that("join_global_damage_cost_factors defaults to the Global region for an unmapped/unknown country code", {
  assets <- make_flood_row("A1", "commercial building", "ZZ", 50)
  result <- suppressMessages(
    join_global_damage_cost_factors(assets, test_global_hazard_config(), test_global_hazards_dir())
  )

  row <- result[result$asset == "A1", ]
  testthat::expect_equal(row$region, "Global")
  testthat::expect_equal(row$damage_factor, 0.055)  # Global commercial building at depth 50
  testthat::expect_true(is.na(row$cost_factor))     # "ZZ" isn't in the cost factors file at all
})

testthat::test_that("join_global_damage_cost_factors leaves damage/business_disruption NA when even Global has no row (and warns)", {
  # US -> North America, but "industrial building" has no rows anywhere, not even Global
  assets <- make_flood_row("A1", "industrial building", "US", 0)

  testthat::expect_message(
    result <- join_global_damage_cost_factors(assets, test_global_hazard_config(), test_global_hazards_dir()),
    "No damage/business-disruption data"
  )

  row <- result[result$asset == "A1", ]
  testthat::expect_true(is.na(row$damage_factor))
  testthat::expect_true(is.na(row$business_disruption))
  testthat::expect_equal(row$cost_factor, 250000)   # cost factor is independent and still resolves
})

testthat::test_that("join_global_damage_cost_factors resolves cost_factor for 'GB' via the source file's 'UK' alias", {
  assets <- make_flood_row("A1", "commercial building", "GB", 0)
  result <- suppressMessages(
    join_global_damage_cost_factors(assets, test_global_hazard_config(), test_global_hazards_dir())
  )

  row <- result[result$asset == "A1", ]
  testthat::expect_equal(row$cost_factor, 120000)  # from the "United Kingdom" / Iso2 "UK" row
})

testthat::test_that("join_global_damage_cost_factors handles Namibia's ISO2 code 'NA' without treating it as missing", {
  assets <- make_flood_row("A1", "commercial building", "NA", 50)
  result <- suppressMessages(
    join_global_damage_cost_factors(assets, test_global_hazard_config(), test_global_hazards_dir())
  )

  row <- result[result$asset == "A1", ]
  testthat::expect_equal(row$region, "Africa")
  # No Africa/commercial rows in the fixture -> falls back to Global, not NA
  testthat::expect_equal(row$damage_factor, 0.055)
})

testthat::test_that("join_global_damage_cost_factors handles multiple assets in one call independently", {
  assets <- dplyr::bind_rows(
    make_flood_row("A1", "commercial building", "BR", 0),
    make_flood_row("A2", "commercial building", "US", 100)
  )
  result <- join_global_damage_cost_factors(assets, test_global_hazard_config(), test_global_hazards_dir())

  a1 <- result[result$asset == "A1", ]
  a2 <- result[result$asset == "A2", ]
  testthat::expect_equal(a1$damage_factor, 0.01)
  testthat::expect_equal(a1$cost_factor, 100000)
  testthat::expect_equal(a2$damage_factor, 0.11)
  testthat::expect_equal(a2$cost_factor, 150000)
})

testthat::test_that("join_damage_cost_factors routes to join_global_damage_cost_factors when hazards_dir is config_global", {
  assets <- make_flood_row("A1", "commercial building", "BR", 50)
  result <- join_damage_cost_factors(assets, test_global_hazard_config(), test_global_hazards_dir())

  row <- result[result$asset == "A1", ]
  testthat::expect_equal(row$damage_factor, 0.05)
  testthat::expect_equal(row$cost_factor, 100000)
})
