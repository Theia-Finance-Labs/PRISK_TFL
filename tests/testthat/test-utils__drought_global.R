# Unit tests of the Global drought engine (see helper-drought-global.R for the fixture).

test_that("normalize_drought_crop maps maize/corn, second maize, soy, wheat and winter wheat", {
  x <- c(
    "Maize", "corn", "Maize (corn)", "Sweet corn", "Maize 2", "maize.2", "Second maize", "Safrinha corn",
    "Soybean", "Soybeans", "Soya beans", "soy", "Soja",
    "Wheat", "spring wheat", "Winter wheat", "Wheat Winter", "Wheat.Winter",
    "Rice", "Banana", "Coffee", "Popcorn plant", "", NA
  )
  expect_equal(
    normalize_drought_crop(x),
    c(
      "maize", "maize", "maize", "maize", "maize2", "maize2", "maize2", "maize2",
      "soybean", "soybean", "soybean", "soybean", "soybean",
      "wheat", "wheat", "winter_wheat", "winter_wheat", "winter_wheat",
      NA, NA, NA, NA, NA, NA
    )
  )
})

test_that("resolve_drought_crop uses the input crop, then the country proxy, then maize", {
  proxy <- tibble::tibble(
    country_name = c("Alphaland", "Betaland", "Gammaland", "United Kingdom"),
    iso2 = c("AA", "BB", "CC", "GB"), iso3 = c("AAA", "BBB", "CCC", "GBR"),
    proxy_crop_input = c("Wheat", "Soya beans", NA, "Wheat"),
    proxy_crop_key = c("wheat", "soybean", NA, "wheat")
  )
  res <- resolve_drought_crop(
    c("Corn", "Banana", NA, NA, "Rice", "Maize 2", NA, NA),
    c("AA", "BB", "bbb", "CC", "ZZ", "AA", "UK", "Alphaland"),
    proxy
  )
  expect_equal(res$crop_key, c("maize", "soybean", "soybean", "maize", "maize", "maize2", "wheat", "wheat"))
  expect_equal(
    res$crop_source,
    c("input", "country_proxy", "country_proxy", "default", "default", "input", "country_proxy", "country_proxy")
  )
  expect_equal(res$damage_crop, c("Corn", "Soybean", "Soybean", "Corn", "Corn", "Corn", "Wheat", "Wheat"))
  expect_equal(res$calendar_file[[6]], "Maize.2.crop.calendar.nc")
})

test_that("the real-format proxy table is read and its crop labels normalised", {
  fx <- make_drought_test_root()
  p <- load_drought_crop_proxy_table(fx$mappings_dir)
  expect_equal(p$iso2, c("AA", "BB", "CC", "DD"))
  expect_equal(p$proxy_crop_key, c("wheat", "soybean", NA, "maize"))
})

test_that("scan_global_drought_maps finds scenario, window, season and bands", {
  fx <- make_drought_test_root()
  s <- scan_global_drought_maps(fx$maps_dir)
  expect_equal(nrow(s$files), 2 * 3 * 4)
  expect_setequal(unique(s$files$season), DROUGHT_SEASONS)
  expect_setequal(unique(s$files$centre_year), c(2030.5, 2040.5, 2050.5))
  expect_equal(nrow(s$bands), 24)
  expect_setequal(unique(s$bands$statistic), DROUGHT_STATISTICS)
  expect_false(any(grepl("n_models", s$bands$band)))
})

test_that("load_hazards_and_inventory registers scenario x season x RP x statistic rows without rasters", {
  fx <- make_drought_test_root()
  inv <- suppressMessages(load_hazards_and_inventory(fx$config_dir, fx$indicators_dir, aggregate_factor = 1L))
  expect_length(inv$hazards, 0)
  d <- inv$inventory
  expect_equal(nrow(d), 2 * 4 * 6 * 4)
  expect_true(all(d$hazard_type == "Drought"))
  expect_setequal(unique(d$season), DROUGHT_SEASONS)
  expect_equal(anyDuplicated(d$hazard_name), 0)
  expect_true(all(grepl("__season=", d$hazard_name)))
  expect_true(is_global_drought_config(inv$configs$Drought))
  expect_equal(global_drought_hazard_types(inv$configs), "Drought")
  expect_equal(inv$configs$Drought$crop_calendar, "crop_calendar/")
})

test_that("a Brazil-style drought config is not treated as the global engine", {
  cfg <- load_hazard_configs(get_test_data_dir("hazards", "config"))
  expect_false(is_global_drought_config(cfg$Drought))
  expect_length(global_drought_hazard_types(cfg), 0)
})

test_that("order_drought_seasons puts DJF, MAM, JJA, SON first and keeps other labels sorted", {
  expect_equal(order_drought_seasons(c("SON", "DJF", "JJA", "MAM")), DROUGHT_SEASONS)
  expect_equal(order_drought_seasons(c("Winter", "Summer", "Autumn")), c("Autumn", "Summer", "Winter"))
})

test_that("extract_global_drought_spei reads the requested season / statistic bands per window", {
  fx <- make_drought_test_root()
  pts <- tibble::tibble(asset = c("P1", "P2", "P3"), longitude = c(0.5, NA, 30), latitude = c(0.5, 1, 30))
  e <- extract_global_drought_spei(pts, fx$maps_dir, "ssp585", seasons = "MAM", statistic = "p90", return_periods = 25)
  expect_equal(nrow(e), 3 * 3)
  v <- e$spei3[e$asset == "P1"][order(e$centre_year[e$asset == "P1"])]
  expect_equal(v, vapply(1:3, function(w) drought_test_value("p90", w, 25, "MAM"), numeric(1)), tolerance = 1e-6)
  expect_true(all(is.na(e$spei3[e$asset %in% c("P2", "P3")])))
  expect_error(extract_global_drought_spei(pts, fx$maps_dir, "ssp999"), "no drought maps")
})

test_that("blend_global_drought_spei blends the bracketing windows linearly", {
  fx <- make_drought_test_root()
  pts <- tibble::tibble(asset = "P1", longitude = 0.5, latitude = 0.5)
  e <- extract_global_drought_spei(pts, fx$maps_dir, "ssp585", seasons = c("DJF", "SON"), return_periods = 10)
  b <- blend_global_drought_spei(e, 2039)
  w <- 0.85
  expect_equal(
    b$spei3[b$season == "SON"],
    (1 - w) * drought_test_value("median", 1, 10, "SON") + w * drought_test_value("median", 2, 10, "SON"),
    tolerance = 1e-6
  )
  expect_equal(blend_global_drought_spei(e, 2026)$spei3[1], drought_test_value("median", 1, 10, "DJF"), tolerance = 1e-6)
  expect_equal(blend_global_drought_spei(e, 2070)$spei3[1], drought_test_value("median", 3, 10, "DJF"), tolerance = 1e-6)
})

test_that("crop calendars: own cell, nearest cell with distance, no coordinates", {
  fx <- make_drought_test_root()
  soy <- load_drought_crop_calendar(file.path(fx$calendar_dir, "Soybeans.crop.calendar.nc"))
  expect_true(all(soy$valid$lat > 0 & soy$valid$lon < 0))
  g <- lookup_drought_growing_seasons(c(-1.2, 1.2, NA), c(0.3, -1.2, 1), soy)
  expect_equal(g$calendar_match, c("cell", "nearest_cell", "no_coordinates"))
  expect_equal(g$calendar_distance_km[[1]], 0)
  # nearest valid soybean cell to (1.2, -1.2) is the centre (-0.25, 0.25)
  expect_equal(c(g$calendar_lon[[2]], g$calendar_lat[[2]]), c(-0.25, 0.25))
  expect_equal(g$calendar_distance_km[[2]], haversine_km(1.2, -1.2, -0.25, 0.25), tolerance = 1e-9)
  expect_equal(g$growing_seasons, c("JJA", "JJA", NA))

  # a point outside the calendar grid is not snapped to the grid edge: nearest valid cell instead
  maize <- load_drought_crop_calendar(file.path(fx$calendar_dir, "Maize.crop.calendar.nc"))
  far <- lookup_drought_growing_seasons(10, 10, maize)
  expect_equal(far$calendar_match, "nearest_cell")
  expect_gt(far$calendar_distance_km, 1000)
  multi <- lookup_drought_growing_seasons(c(-1, 1), c(0, 0), maize)
  expect_equal(multi$growing_seasons, c("DJF", "DJF|MAM"))
})

test_that("haversine_km is a great-circle distance", {
  expect_equal(haversine_km(0, 0, 0, 1), 6371.0088 * pi / 180, tolerance = 1e-9)
  expect_equal(haversine_km(10, 50, 10, 50), 0)
})

test_that("the damage function is read, negative values clamped and matched to the closest step", {
  fx <- make_drought_test_root()
  tab <- load_drought_damage_function(fx$mappings_dir)
  expect_true(all(tab$damage_factor >= 0))
  expect_equal(tab$damage_factor[tab$crop == "Wheat" & tab$spei3 == -0.1], 0)
  spei <- c(-1.345, -1.36, -3.8, 0.7, -0.04, NA, -0.1)
  crop <- c("Corn", "Corn", "Soybean", "Soybean", "Wheat", "Corn", "Wheat")
  d <- lookup_drought_damage(spei, crop, tab)
  expect_equal(d$spei3_matched, c(-1.3, -1.4, -3, 0, 0, NA, -0.1))
  expect_equal(d$damage_factor_in_window, c(0.065, 0.07, 0.3, 0, 0, NA, 0))
  expect_equal(d$off_window, rep(0.15, 7))
})

test_that("the damage function file is validated", {
  d <- withr::local_tempdir()
  writexl::write_xlsx(data.frame(SPEI3 = -1, `damage factor` = 0.1, crop = "Corn", off_window = 0.15, check.names = FALSE), file.path(d, "f.xlsx"))
  expect_error(load_drought_damage_function(d, "f.xlsx"), "no rows for crop")
  writexl::write_xlsx(data.frame(SPEI3 = -1, crop = "Corn", check.names = FALSE), file.path(d, "g.xlsx"))
  expect_error(load_drought_damage_function(d, "g.xlsx"), "missing column")
})

test_that("effective damage: full in the growing period, off-window share outside", {
  expect_equal(drought_effective_damage(c(0.2, 0.2, 0.2), c(TRUE, FALSE, NA), 0.15), c(0.2, 0.03, 0.03))
})

test_that("build_global_drought_event_factors builds one row per asset with the expected damage", {
  fx <- make_drought_test_root()
  a <- drought_test_assets()
  ev <- tibble::tibble(
    event_id = "ev1", hazard_type = "Drought", hazard_name = "x", scenario_name = "ssp585",
    season = "DJF", return_period = 10, event_year = 2035L, statistic = "median"
  )
  out <- build_global_drought_event_factors(a, ev, fx$configs, fx$config_dir)
  r <- out$rows
  expect_equal(nrow(r), nrow(a))
  spei <- 0.55 * drought_test_value("median", 1, 10, "DJF") + 0.45 * drought_test_value("median", 2, 10, "DJF")
  get <- function(asset, col) r[[col]][r$asset == asset]

  expect_equal(get("A1", "spei3"), spei, tolerance = 1e-6)
  expect_true(get("A1", "in_growing_period"))
  expect_equal(get("A1", "damage_factor"), drought_test_damage(spei, "Corn"), tolerance = 1e-9)
  # soybean (input) and Banana -> Betaland proxy soybean: nearest calendar is JJA -> off-window in DJF
  for (x in c("A2", "A3")) {
    expect_false(get(x, "in_growing_period"))
    expect_equal(get(x, "damage_factor"), 0.15 * drought_test_damage(spei, "Soybean"), tolerance = 1e-9)
    expect_equal(get(x, "calendar_match"), "nearest_cell")
  }
  expect_equal(get("A3", "crop_source"), "country_proxy")
  expect_equal(get("A4", "crop_source"), "default")
  expect_equal(get("A4", "crop"), "Maize")
  expect_equal(get("A5", "damage_factor"), 0)
  expect_true(is.na(get("A5", "crop")))
  expect_match(get("A5", "drought_status"), "non-agriculture")
  expect_equal(get("A6", "damage_factor"), 0)
  expect_match(get("A6", "drought_status"), "no drought-map value")
  expect_equal(get("A7", "damage_factor"), drought_test_damage(spei, "Wheat"), tolerance = 1e-9)
  expect_equal(get("A8", "calendar_crop"), "Maize.2")
  expect_equal(get("A8", "damage_factor"), 0.15 * drought_test_damage(spei, "Corn"), tolerance = 1e-9)
  expect_equal(get("A9", "damage_factor"), 0)

  w <- paste(out$warnings, collapse = "\n")
  expect_match(w, "non-agriculture asset\\(s\\) A5")
  expect_match(w, "A3 \\(Banana -> Soybean\\)")
  expect_match(w, "no country proxy crop for asset\\(s\\) A4")
  expect_match(w, "nearest calendar cell")
  expect_match(w, "A6")
  expect_match(w, "A9")
})

test_that("a drought event without a valid season is rejected", {
  fx <- make_drought_test_root()
  ev <- tibble::tibble(
    event_id = "ev1", hazard_type = "Drought", hazard_name = "x", scenario_name = "ssp585",
    season = "Summer", return_period = 10, event_year = 2035L, statistic = "median"
  )
  expect_error(build_global_drought_event_factors(drought_test_assets(), ev, fx$configs, fx$config_dir), "needs a season")
})

test_that("aal_weights_for_rps reproduces calculate_aal for any loss curve", {
  rps <- c(2, 5, 10, 25, 50, 100)
  w <- aal_weights_for_rps(rps)
  set.seed(1)
  for (i in 1:5) {
    loss <- sort(runif(6))
    expect_equal(sum(w * loss), calculate_aal(rps, loss), tolerance = 1e-12)
  }
})

test_that("compute_global_drought_aal = mean over seasons of the per-season AAL, per year", {
  fx <- make_drought_test_root()
  a <- drought_test_assets()
  ev <- tibble::tibble(hazard_type = "Drought", scenario_name = "ssp585", statistic = "median")
  res <- compute_global_drought_aal(a, ev, fx$configs, fx$config_dir, years = c(2026, 2035, 2060))
  expect_setequal(names(res), c("summary", "yearly", "detail", "factors_by_rp", "warnings"))
  expect_equal(nrow(res$yearly), nrow(a) * 3)

  rps <- drought_test_rps()
  hand <- function(year, crop, in_seasons) {
    wts <- heat_blend_weights(year, c(2030.5, 2040.5, 2050.5))
    season_aal <- vapply(DROUGHT_SEASONS, function(s) {
      loss <- vapply(rps, function(rp) {
        spei <- sum(wts$weight * vapply(match(wts$centre_year, c(2030.5, 2040.5, 2050.5)), function(wi) drought_test_value("median", wi, rp, s), numeric(1)))
        drought_test_damage(spei, crop) * if (s %in% in_seasons) 1 else 0.15
      }, numeric(1))
      calculate_aal(rps, loss)
    }, numeric(1))
    mean(season_aal)
  }
  y <- res$yearly
  rate <- function(asset, year) y$aal_drought_revenue_rate[y$asset == asset & y$year == year]
  for (yr in c(2026, 2035, 2060)) {
    expect_equal(rate("A1", yr), hand(yr, "Corn", "DJF"), tolerance = 1e-9)
    # soybean uses its nearest calendar cell (JJA); winter wheat spans three seasons
    expect_equal(rate("A2", yr), hand(yr, "Soybean", "JJA"), tolerance = 1e-9)
    expect_equal(rate("A7", yr), hand(yr, "Wheat", c("DJF", "MAM", "JJA")), tolerance = 1e-9)
    expect_equal(rate("A5", yr), 0)
    expect_equal(rate("A6", yr), 0)
  }
  # later windows are drier -> AAL rises over time
  expect_gt(rate("A1", 2060), rate("A1", 2026))
  expect_equal(res$summary$aal_drought_revenue_rate[res$summary$asset == "A1"], mean(y$aal_drought_revenue_rate[y$asset == "A1"]))

  # zero loss at RP 1: the AAL is below the RP2 loss weighted over the whole curve
  expect_true(all(res$detail$channel == "aal_drought_revenue_rate"))
  fb <- res$factors_by_rp
  expect_true(all(c("drought_loss_rate", "spei3_DJF", "spei3_SON", "crop", "growing_seasons") %in% names(fb)))
  expect_setequal(unique(fb$asset), c("A1", "A2", "A3", "A4", "A6", "A7", "A8", "A9"))
})

test_that("a longer growing period means a higher drought AAL for the same hazard", {
  fx <- make_drought_test_root()
  a <- tibble::tibble(
    asset = c("short", "long"), company = "C", longitude = c(-1, 1), latitude = c(0, 0),
    country = "AA", asset_subtype = "Maize", asset_category = "agriculture", share_of_economic_activity = 1
  )
  ev <- tibble::tibble(hazard_type = "Drought", scenario_name = "ssp585", statistic = "median")
  res <- compute_global_drought_aal(a, ev, fx$configs, fx$config_dir, years = 2035)
  s <- res$summary
  expect_gt(s$aal_drought_revenue_rate[s$asset == "long"], s$aal_drought_revenue_rate[s$asset == "short"])
})

test_that("compute_aal_shock_trajectories subtracts yearly heat and drought rates additively", {
  base <- tibble::tibble(asset = "A", company = "C", year = 2030:2031, revenue = c(100, 200), profit = c(10, 20))
  ch <- tibble::tibble(asset = "A", aal_pd_profit = 0, aal_pd_revenue_rate = 0.01, aal_bd_revenue_rate = 0.02)
  comp <- tibble::tibble(company = "C", net_profit_margin = 0.1)
  heat <- tibble::tibble(asset = "A", year = 2030:2031, aal_lp_revenue_rate = c(0.03, 0.04))
  dr <- tibble::tibble(asset = "A", year = 2030:2031, aal_drought_revenue_rate = c(0.05, 0.10))
  out <- compute_aal_shock_trajectories(base, ch, comp, aal_heat_yearly = heat, aal_drought_yearly = dr)
  expect_equal(out$revenue, c(100 * (1 - 0.11), 200 * (1 - 0.17)))
  only_dr <- compute_aal_shock_trajectories(base, ch, comp, aal_drought_yearly = dr)
  expect_equal(only_dr$revenue, c(100 * (1 - 0.08), 200 * (1 - 0.13)))
  none <- compute_aal_shock_trajectories(base, ch, comp)
  expect_equal(none$revenue, c(100 * 0.97, 200 * 0.97))
})

test_that("global_adm_hazard_types lists the global heat and drought hazards", {
  fx <- make_drought_test_root()
  hx <- make_heat_test_root()
  cfg <- c(fx$configs, hx$configs)
  expect_setequal(global_adm_hazard_types(cfg), c("Heat", "Drought"))
})

test_that("build_probabilistic_assets_table lays out the drought AAL per asset", {
  fx <- make_drought_test_root()
  a <- drought_test_assets()
  ev <- tibble::tibble(hazard_type = "Drought", scenario_name = "ssp585", statistic = "median")
  res <- compute_global_drought_aal(a, ev, fx$configs, fx$config_dir, years = 2030:2032)
  base <- tibble::tibble(asset = rep(a$asset, each = 3), year = rep(2030:2032, nrow(a)), revenue = 1000)
  res <- add_drought_aal_revenue_adjustment(res, base)
  tab <- build_probabilistic_assets_table(res$factors_by_rp, res$summary, hazard_type_val = "Drought")
  expect_equal(nrow(tab), 8)
  expect_true(all(c(
    "crop", "growing_seasons", "SPEI-3 DJF (RP=10)", "Yield Loss Rate (mean over seasons) (RP=100)",
    "AAL Drought Yield Loss - Revenue Rate (mean over years)", "Average Annual Revenue Adjustment ($)"
  ) %in% names(tab)))
  a1 <- tab[tab$asset == "A1", ]
  expect_equal(
    a1[["Average Annual Revenue Adjustment ($)"]],
    round(-1000 * res$summary$aal_drought_revenue_rate[res$summary$asset == "A1"], 0)
  )
})

test_that("assumed crops are labelled in the display, input crops are not", {
  df <- tibble::tibble(
    asset = c("A", "B", "C", "D"),
    crop_input = c("Maize", "Banana", NA, "Office"),
    crop = c("Maize", "Soybean", "Maize", NA),
    calendar_crop = c("Maize", "Soybeans", "Maize", NA),
    crop_source = c("input", "country_proxy", "default", NA)
  )
  out <- label_assumed_drought_crop(df)
  expect_equal(out$crop, c("Maize", "NA (assumed Soybean)", "NA (assumed Maize)", NA))
  expect_equal(out$calendar_crop, c("Maize", "NA (assumed Soybeans)", "NA (assumed Maize)", NA))
  expect_match(out$crop_source[[2]], "^assumed: most produced")
  expect_match(out$crop_source[[3]], "^assumed: maize")
  expect_equal(out$crop_source[[1]], "input")
  expect_identical(label_assumed_drought_crop(tibble::tibble(x = 1)), tibble::tibble(x = 1))
})
