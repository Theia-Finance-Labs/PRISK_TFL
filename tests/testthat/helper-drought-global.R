# Synthetic fixtures for the Global drought (crop yield) engine.
#
# Builds a tiny data root:
#   <root>/hazards/config_global/Drought.yml
#   <root>/hazards/indicators/drought_maps/spei3_rl_<ssp>_<start>-<end>_<season>.tif   (29 bands)
#   <root>/hazards/indicators/crop_calendar/<Crop>.crop.calendar.nc                      (0.5 deg)
#   <root>/hazards/mappings/drought_damage_function_global.xlsx
#   <root>/hazards/mappings/Most used Crop Country List_WSM.xlsx
#
# Map values are uniform over the 4 x 4 grid (lon/lat -2..2) and deterministic, so results
# can be computed by hand:
#   rl_<stat>_RP<n> = base_stat - 0.4 * rp_index - 0.1 * window_index - season_offset - ssp_offset
# base_stat: median 0, mean -0.02, p10 +0.3, p90 -0.3;  season_offset: DJF 0, MAM .03, JJA .06, SON .09
# (offsets chosen so no value falls exactly half-way between two 0.1 SPEI steps)
# rp_index 1..6 for RP 2, 5, 10, 25, 50, 100; window_index 1..3; ssp126 is 0.2 wetter (ssp_offset -0.2).
#
# Crop calendars (cell centres lon/lat -1.75..1.75):
#   Maize        everywhere; lon < 0 -> DJF, lon > 0 -> DJF + MAM
#   Maize.2      lon < 0 only -> MAM
#   Soybeans     only lat > 0 and lon < 0 -> JJA (elsewhere no calendar: nearest-cell fallback)
#   Wheat        only lat < 0 -> SON
#   Wheat.Winter everywhere -> DJF + MAM + JJA
#
# Damage curves (SPEI -3..0, step 0.1): Corn -0.05 * spei, Soybean -0.1 * spei,
# Wheat -0.1 * spei except -0.002 at SPEI -0.1 (negative, clamped to 0), Rice -0.2 * spei.
# off_window 0.15 for every crop.

drought_test_windows <- function() {
  data.frame(start = c(2021L, 2031L, 2041L), end = c(2040L, 2050L, 2060L), centre = c(2030.5, 2040.5, 2050.5))
}

drought_test_rps <- function() c(2, 5, 10, 25, 50, 100)

drought_test_value <- function(stat, window_index, rp, season, ssp = "ssp585") {
  base <- c(median = 0, mean = -0.02, p10 = 0.3, p90 = -0.3)[[stat]]
  s_off <- c(DJF = 0, MAM = 0.03, JJA = 0.06, SON = 0.09)[[season]]
  ssp_off <- if (identical(ssp, "ssp126")) -0.2 else 0
  base - 0.4 * match(rp, drought_test_rps()) - 0.1 * window_index - s_off - ssp_off
}

make_drought_test_maps <- function(maps_dir, ssps = c("ssp585", "ssp126")) {
  dir.create(maps_dir, recursive = TRUE, showWarnings = FALSE)
  wins <- drought_test_windows()
  rps <- drought_test_rps()
  stats <- c("median", "mean", "p10", "p90")
  band_grid <- expand.grid(rp = rps, stat = stats, stringsAsFactors = FALSE)
  band_names <- c(
    paste0("rl_", band_grid$stat, "_RP", band_grid$rp),
    "n_models", "n_saturated", "n_censored", "n_lmom", "share_capped_median"
  )
  for (ssp in ssps) {
    for (season in c("DJF", "MAM", "JJA", "SON")) {
      for (w in seq_len(nrow(wins))) {
        layers <- lapply(seq_len(nrow(band_grid)), function(i) {
          r <- terra::rast(nrows = 4, ncols = 4, xmin = -2, xmax = 2, ymin = -2, ymax = 2, crs = "EPSG:4326")
          terra::values(r) <- drought_test_value(band_grid$stat[[i]], w, band_grid$rp[[i]], season, ssp)
          r
        })
        extra <- lapply(1:5, function(i) {
          r <- terra::rast(nrows = 4, ncols = 4, xmin = -2, xmax = 2, ymin = -2, ymax = 2, crs = "EPSG:4326")
          terra::values(r) <- 23
          r
        })
        stack <- do.call(c, c(layers, extra))
        names(stack) <- band_names
        terra::writeRaster(
          stack,
          file.path(maps_dir, sprintf("spei3_rl_%s_%d-%d_%s.tif", ssp, wins$start[[w]], wins$end[[w]], season)),
          overwrite = TRUE, datatype = "FLT4S"
        )
      }
    }
  }
  invisible(maps_dir)
}

#' Write one synthetic crop calendar; flag_fun(lon, lat) returns 4 logicals (DJF..SON) or NULL
write_drought_test_calendar <- function(path, flag_fun) {
  lon <- seq(-1.75, 1.75, by = 0.5)
  lat <- seq(1.75, -1.75, by = -0.5)
  d_lon <- ncdf4::ncdim_def("longitude", "degrees_east", lon)
  d_lat <- ncdf4::ncdim_def("latitude", "degrees_north", lat)
  d_season <- ncdf4::ncdim_def("season", "season (1=DJF, 2=MAM, 3=JJA, 4=SON)", 1:4)
  v_gs <- ncdf4::ncvar_def("grow.season", "flag", list(d_lon, d_lat, d_season), missval = -999, prec = "short")
  v_gd <- ncdf4::ncvar_def("grow.days", "days", list(d_lon, d_lat), missval = 9e20, prec = "float")
  flags <- array(NA_integer_, c(length(lon), length(lat), 4))
  days <- matrix(NA_real_, length(lon), length(lat))
  for (i in seq_along(lon)) {
    for (j in seq_along(lat)) {
      f <- flag_fun(lon[[i]], lat[[j]])
      if (!is.null(f)) {
        flags[i, j, ] <- as.integer(f)
        days[i, j] <- 90 * sum(f)
      }
    }
  }
  nc <- ncdf4::nc_create(path, list(v_gs, v_gd))
  ncdf4::ncvar_put(nc, v_gs, flags)
  ncdf4::ncvar_put(nc, v_gd, days)
  ncdf4::nc_close(nc)
  invisible(path)
}

make_drought_test_calendars <- function(cal_dir) {
  dir.create(cal_dir, recursive = TRUE, showWarnings = FALSE)
  write_drought_test_calendar(file.path(cal_dir, "Maize.crop.calendar.nc"), function(lon, lat) {
    if (lon < 0) c(TRUE, FALSE, FALSE, FALSE) else c(TRUE, TRUE, FALSE, FALSE)
  })
  write_drought_test_calendar(file.path(cal_dir, "Maize.2.crop.calendar.nc"), function(lon, lat) {
    if (lon < 0) c(FALSE, TRUE, FALSE, FALSE) else NULL
  })
  write_drought_test_calendar(file.path(cal_dir, "Soybeans.crop.calendar.nc"), function(lon, lat) {
    if (lat > 0 && lon < 0) c(FALSE, FALSE, TRUE, FALSE) else NULL
  })
  write_drought_test_calendar(file.path(cal_dir, "Wheat.crop.calendar.nc"), function(lon, lat) {
    if (lat < 0) c(FALSE, FALSE, FALSE, TRUE) else NULL
  })
  write_drought_test_calendar(file.path(cal_dir, "Wheat.Winter.crop.calendar.nc"), function(lon, lat) {
    c(TRUE, TRUE, TRUE, FALSE)
  })
  invisible(cal_dir)
}

drought_test_damage_table <- function() {
  spei <- round(seq(-3, 0, by = 0.1), 1)
  wheat <- -0.1 * spei
  wheat[spei == -0.1] <- -0.002
  data.frame(
    SPEI3 = rep(spei, 4),
    `damage factor` = c(-0.05 * spei, -0.1 * spei, wheat, -0.2 * spei),
    crop = rep(c("Corn", "Soybean", "Wheat", "Rice"), each = length(spei)),
    off_window = 0.15,
    check.names = FALSE
  )
}

#' Closed-form in-window damage for a SPEI value (closest 0.1 step, -3..0)
drought_test_damage <- function(spei, crop) {
  s <- min(0, max(-3, round(spei, 1)))
  d <- switch(crop, Corn = -0.05 * s, Soybean = -0.1 * s, Wheat = -0.1 * s, Rice = -0.2 * s)
  if (crop == "Wheat" && isTRUE(all.equal(s, -0.1))) d <- 0
  max(0, d)
}

drought_test_proxy_table <- function() {
  data.frame(
    Country = c("Alphaland", "Betaland", "Gammaland", "Deltaland"),
    ISO2 = c("AA", "BB", "CC", "DD"),
    ISO3 = c("AAA", "BBB", "CCC", "DDD"),
    `Top Crop` = c("Wheat", "Soya beans", NA, "Maize (corn)"),
    check.names = FALSE
  )
}

drought_test_config_yaml <- function() {
  c(
    "indicators:",
    "  spei3:",
    "    file: drought_maps/",
    "    type: global_drought_maps",
    "    variable: spei3",
    "    index: [scenario_name, season, return_period, statistic]",
    "    agg: closest",
    "index_indicator: spei3",
    "crop_calendar: crop_calendar/",
    "spatial_separation_scheme: global_adm",
    "mappings:",
    "  damage_function:",
    "    file: drought_damage_function_global.xlsx",
    "  crop_proxy:",
    "    file: Most used Crop Country List_WSM.xlsx",
    "shocks:",
    "  revenue:",
    "    equations:",
    "      - name: drought_agriculture_global",
    "        when: \"tolower(asset_category) == 'agriculture'\"",
    "        equation: \"revenue ~ revenue * (1 - damage_factor)\""
  )
}

#' Build the synthetic data root and return its paths and loaded configs
make_drought_test_root <- function(root = withr::local_tempdir(.local_envir = parent.frame())) {
  config_dir <- file.path(root, "hazards", "config_global")
  indicators_dir <- file.path(root, "hazards", "indicators")
  mappings_dir <- file.path(root, "hazards", "mappings")
  dir.create(config_dir, recursive = TRUE)
  dir.create(mappings_dir, recursive = TRUE)
  writeLines(drought_test_config_yaml(), file.path(config_dir, "Drought.yml"))
  make_drought_test_maps(file.path(indicators_dir, "drought_maps"))
  make_drought_test_calendars(file.path(indicators_dir, "crop_calendar"))
  writexl::write_xlsx(drought_test_damage_table(), file.path(mappings_dir, "drought_damage_function_global.xlsx"))
  writexl::write_xlsx(drought_test_proxy_table(), file.path(mappings_dir, "Most used Crop Country List_WSM.xlsx"))
  list(
    root = root,
    config_dir = config_dir,
    indicators_dir = indicators_dir,
    mappings_dir = mappings_dir,
    maps_dir = file.path(indicators_dir, "drought_maps"),
    calendar_dir = file.path(indicators_dir, "crop_calendar"),
    configs = load_hazard_configs(config_dir)
  )
}

#' Assets covering every crop / calendar / category branch (see the header)
drought_test_assets <- function() {
  tibble::tibble(
    asset = c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9"),
    company = c("C1", "C1", "C1", "C2", "C2", "C2", "C3", "C3", "C3"),
    longitude = c(-1.2, 1.2, 0.6, -0.6, 0.5, NA, 1.5, -1.5, 10),
    latitude = c(0.3, -1.2, 0.6, -0.6, 0.5, NA, 1.5, -1.5, 10),
    country = c("AA", "AA", "BB", "CC", "AA", "AA", "DD", "BB", "ZZ"),
    asset_subtype = c("Maize", "Soybean", "Banana", NA, NA, "Corn", "Winter wheat", "Maize 2", "maize"),
    asset_category = c(rep("agriculture", 4), "commercial building", rep("agriculture", 4)),
    share_of_economic_activity = 1
  )
}
