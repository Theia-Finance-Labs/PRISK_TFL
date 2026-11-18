# Global drought hazard (agricultural yield loss) -- Global mode only.
#
# Brazil's Drought hazard (SPI-3 NetCDF, GWL-indexed, state x crop factor table with
# its own seasons) is completely untouched: everything here is gated on a hazard
# config whose indicator is declared `type: global_drought_maps`
# (see `{base_dir}/hazards/config_global/Drought.yml`).
#
# Data
#   * SPEI-3 return-level maps: one GeoTIFF per SSP x 20-year window x season
#     (`spei3_rl_<ssp>_<start>-<end>_<DJF|MAM|JJA|SON>.tif`), bands
#     `rl_<median|mean|p10|p90>_RP<n>` = SPEI-3 value reached in a 1-in-RP-year drought
#     (lower = drier). Each window represents the climate of its centre year; a shock
#     year between two centres is blended linearly (same rule as global heat,
#     `heat_blend_weights()`).
#   * Crop calendars (`<Crop>.crop.calendar.nc`, 0.5 deg): `grow.season` flags, per
#     cell, which seasons fall in the crop's growing period.
#   * Damage function (`drought_damage_function_global.xlsx`): yield loss per SPEI-3
#     step (0.1) for Corn / Soybean / Wheat, plus the off-window share.
#   * Crop proxy (`Most used Crop Country List_WSM.xlsx`): the most produced of
#     wheat / soy / maize per country, used when the asset's crop is unknown.
#
# Impact: agriculture assets only. revenue_shock = revenue x (1 - damage_factor) with
#   damage_factor = f_crop(SPEI-3)                 if the event season is in the growing period
#   damage_factor = f_crop(SPEI-3) x off_window    otherwise (off_window = 0.15)

#' Seasons of the global drought maps, in calendar order (internal)
#' @noRd
DROUGHT_SEASONS <- c("DJF", "MAM", "JJA", "SON")

#' Drought-map statistics available as bands (median is the default)
#' @noRd
DROUGHT_STATISTICS <- c("median", "mean", "p10", "p90")

#' Crop assumed when the asset crop is unknown and its country has no proxy crop
#' @noRd
DROUGHT_DEFAULT_CROP <- "maize"

#' Supported crops: calendar file and damage-function crop (internal)
#'
#' @description `maize2` (second-season maize) and `winter_wheat` are only used when
#'   the asset input states them explicitly; they share the Corn / Wheat damage curves.
#' @noRd
drought_crop_table <- function() {
  tibble::tibble(
    crop_key = c("maize", "maize2", "soybean", "wheat", "winter_wheat"),
    crop_label = c("Maize", "Maize 2", "Soybean", "Wheat", "Winter wheat"),
    calendar_file = c(
      "Maize.crop.calendar.nc", "Maize.2.crop.calendar.nc", "Soybeans.crop.calendar.nc",
      "Wheat.crop.calendar.nc", "Wheat.Winter.crop.calendar.nc"
    ),
    damage_crop = c("Corn", "Corn", "Soybean", "Wheat", "Wheat")
  )
}

# ---------------------------------------------------------------------------
# Config helpers
# ---------------------------------------------------------------------------

#' TRUE when a hazard config describes the global drought-map engine (internal)
#' @noRd
is_global_drought_config <- function(cfg) {
  if (is.null(cfg) || is.null(cfg$indicators) || length(cfg$indicators) == 0) {
    return(FALSE)
  }
  any(vapply(
    cfg$indicators,
    function(ind) identical(as.character(ind$type), "global_drought_maps"),
    logical(1)
  ))
}

#' Names of hazard types that use the global drought-map engine (internal)
#' @noRd
global_drought_hazard_types <- function(hazard_configs) {
  if (is.null(hazard_configs) || length(hazard_configs) == 0) {
    return(character(0))
  }
  names(hazard_configs)[vapply(hazard_configs, is_global_drought_config, logical(1))]
}

#' Logical vector: which event rows are global-drought events (internal)
#' @noRd
is_global_drought_event <- function(events, hazard_configs) {
  if (is.null(events) || nrow(events) == 0 || !"hazard_type" %in% names(events)) {
    return(logical(0))
  }
  as.character(events$hazard_type) %in% global_drought_hazard_types(hazard_configs)
}

#' Hazard types using the global administrative-region scheme (Heat, Drought) (internal)
#' @noRd
global_adm_hazard_types <- function(hazard_configs) {
  unique(c(global_heat_hazard_types(hazard_configs), global_drought_hazard_types(hazard_configs)))
}

#' Order seasons for display: DJF, MAM, JJA, SON first, anything else sorted after (internal)
#' @noRd
order_drought_seasons <- function(x) {
  x <- as.character(x)
  c(intersect(DROUGHT_SEASONS, x), sort(setdiff(x, DROUGHT_SEASONS)))
}

#' The global drought indicator of a config (internal)
#' @noRd
global_drought_indicator <- function(hazard_config) {
  Filter(function(ind) identical(as.character(ind$type), "global_drought_maps"), hazard_config$indicators)[[1]]
}

#' Resolve the SPEI-3 maps folder from a hazards config directory (internal)
#' @param hazards_dir Path to `.../hazards/config_global`
#' @noRd
global_drought_maps_dir <- function(hazards_dir, hazard_config) {
  indicator <- global_drought_indicator(hazard_config)
  file.path(dirname(hazards_dir), "indicators", gsub("/+$", "", indicator$file))
}

#' Resolve the crop-calendar folder (internal)
#'
#' @description Read from the hazard config's `crop_calendar` entry (relative to
#'   `hazards/indicators`), defaulting to `Drought Global Crop Calendar`.
#' @noRd
global_drought_calendar_dir <- function(hazards_dir, hazard_config) {
  rel <- hazard_config$crop_calendar
  if (is.null(rel) || !nzchar(as.character(rel))) rel <- "Drought Global Crop Calendar"
  file.path(dirname(hazards_dir), "indicators", gsub("/+$", "", as.character(rel)))
}

#' Mapping file name from the config, with a default (internal)
#' @noRd
global_drought_mapping_file <- function(hazard_config, key, default) {
  f <- hazard_config$mappings[[key]]$file
  if (is.null(f) || !nzchar(as.character(f))) default else as.character(f)
}

# ---------------------------------------------------------------------------
# Map discovery, inventory
# ---------------------------------------------------------------------------

#' Discover global SPEI-3 return-level maps and their bands (internal)
#'
#' @param maps_dir Folder holding `spei3_rl_<ssp>_<start>-<end>_<season>.tif`
#' @return list(files = tibble(ssp, start_year, end_year, centre_year, season, path),
#'   bands = tibble(statistic, return_period, band))
#' @noRd
scan_global_drought_maps <- function(maps_dir) {
  empty <- list(
    files = tibble::tibble(
      ssp = character(), start_year = integer(), end_year = integer(),
      centre_year = numeric(), season = character(), path = character()
    ),
    bands = tibble::tibble(statistic = character(), return_period = numeric(), band = character())
  )
  if (is.null(maps_dir) || !dir.exists(maps_dir)) {
    return(empty)
  }
  rx <- "^spei3_rl_(.+)_([0-9]{4})-([0-9]{4})_(DJF|MAM|JJA|SON)\\.tif$"
  paths <- list.files(maps_dir, pattern = rx, full.names = TRUE)
  if (length(paths) == 0) {
    return(empty)
  }
  m <- regmatches(basename(paths), regexec(rx, basename(paths)))
  files <- tibble::tibble(
    ssp = vapply(m, `[[`, character(1), 2),
    start_year = as.integer(vapply(m, `[[`, character(1), 3)),
    end_year = as.integer(vapply(m, `[[`, character(1), 4)),
    season = vapply(m, `[[`, character(1), 5),
    path = paths
  ) |>
    dplyr::mutate(centre_year = (.data$start_year + .data$end_year) / 2) |>
    dplyr::arrange(.data$ssp, match(.data$season, DROUGHT_SEASONS), .data$centre_year) |>
    dplyr::select("ssp", "start_year", "end_year", "centre_year", "season", "path")

  band_names <- names(terra::rast(files$path[[1]]))
  bm <- regmatches(band_names, regexec("^rl_(median|mean|p10|p90)_RP([0-9]+)$", band_names))
  bm <- bm[lengths(bm) == 3]
  bands <- tibble::tibble(
    statistic = vapply(bm, `[[`, character(1), 2),
    return_period = as.numeric(vapply(bm, `[[`, character(1), 3)),
    band = vapply(bm, `[[`, character(1), 1)
  )
  list(files = files, bands = bands)
}

#' Build the global Drought inventory rows for the UI / event reconciliation (internal)
#'
#' @description One row per (scenario, season, return period, statistic). As for heat,
#'   the map window is not an inventory dimension: it follows from the shock year.
#' @noRd
build_global_drought_inventory <- function(hazard_type, indicator_key, indicator, hazard_indicators_dir) {
  maps_dir <- file.path(hazard_indicators_dir, gsub("/+$", "", indicator$file))
  scanned <- scan_global_drought_maps(maps_dir)
  if (nrow(scanned$files) == 0 || nrow(scanned$bands) == 0) {
    message("  No global drought maps found at: ", maps_dir)
    return(NULL)
  }

  grid <- scanned$files |>
    dplyr::distinct(scenario_name = .data$ssp, .data$season) |>
    tidyr::crossing(scanned$bands |> dplyr::select("return_period", "statistic")) |>
    dplyr::arrange(.data$scenario_name, match(.data$season, DROUGHT_SEASONS), .data$statistic, .data$return_period)

  if (all(grid$return_period == round(grid$return_period))) {
    grid$return_period <- as.integer(grid$return_period)
  }

  grid |>
    dplyr::mutate(
      hazard_type = hazard_type,
      hazard_indicator = indicator_key,
      indicator_key = paste0(
        "drought_global__", indicator$variable,
        "__scenario_name=", .data$scenario_name,
        "__season=", .data$season,
        "__return_period=", .data$return_period,
        "__statistic=", .data$statistic
      ),
      hazard_name = paste0(hazard_type, "__", .data$indicator_key),
      ensemble = "mean",
      source = "drought_maps",
      agg = indicator$agg,
      categorical = FALSE,
      variable = indicator$variable
    ) |>
    dplyr::select(
      "hazard_type", "hazard_indicator", "hazard_name", "indicator_key", "ensemble", "season",
      "source", "agg", "categorical", "variable", "scenario_name", "return_period", "statistic"
    )
}

# ---------------------------------------------------------------------------
# SPEI-3 extraction and time blending
# ---------------------------------------------------------------------------

#' Extract SPEI-3 return levels per asset, season, window and return period (internal)
#'
#' @param assets data.frame with columns asset, longitude, latitude
#' @param maps_dir drought maps folder
#' @param ssp scenario, e.g. "ssp585"
#' @param seasons character vector of seasons to read (default all four)
#' @param statistic one of DROUGHT_STATISTICS
#' @param return_periods numeric vector (default: all available)
#' @return tibble(asset, season, centre_year, return_period, spei3); NA for assets
#'   without usable coordinates or outside the map (ocean, south of 60S)
#' @noRd
extract_global_drought_spei <- function(assets, maps_dir, ssp, seasons = DROUGHT_SEASONS,
                                        statistic = "median", return_periods = NULL) {
  scanned <- scan_global_drought_maps(maps_dir)
  files <- scanned$files[scanned$files$ssp == ssp & scanned$files$season %in% seasons, , drop = FALSE]
  if (nrow(files) == 0) {
    stop(
      "extract_global_drought_spei: no drought maps found for scenario '", ssp, "' and season(s) ",
      paste(seasons, collapse = ", "), " in ", maps_dir
    )
  }
  if (!statistic %in% DROUGHT_STATISTICS) {
    stop("extract_global_drought_spei: statistic must be one of: ", paste(DROUGHT_STATISTICS, collapse = ", "))
  }
  bands <- scanned$bands[scanned$bands$statistic == statistic, , drop = FALSE]
  if (!is.null(return_periods)) {
    bands <- bands[bands$return_period %in% as.numeric(return_periods), , drop = FALSE]
  }
  if (nrow(bands) == 0) {
    stop("extract_global_drought_spei: no bands for statistic '", statistic, "' and the requested return periods")
  }

  lon <- suppressWarnings(as.numeric(assets$longitude))
  lat <- suppressWarnings(as.numeric(assets$latitude))
  ok <- !is.na(lon) & !is.na(lat)

  out <- vector("list", nrow(files))
  for (i in seq_len(nrow(files))) {
    vals <- matrix(NA_real_, nrow = nrow(assets), ncol = nrow(bands))
    if (any(ok)) {
      r <- terra::rast(files$path[[i]])[[bands$band]]
      ex <- terra::extract(r, cbind(lon[ok], lat[ok]))
      vals[ok, ] <- as.matrix(ex[, bands$band, drop = FALSE])
    }
    vals[!is.finite(vals)] <- NA_real_
    out[[i]] <- tibble::tibble(
      asset = rep(as.character(assets$asset), times = nrow(bands)),
      season = files$season[[i]],
      centre_year = files$centre_year[[i]],
      return_period = rep(bands$return_period, each = nrow(assets)),
      spei3 = as.vector(vals)
    )
  }
  dplyr::bind_rows(out)
}

#' Blend per-window SPEI-3 to one value per asset, season and return period (internal)
#' @param spei_long output of `extract_global_drought_spei()`
#' @param year calendar year
#' @return tibble(asset, season, return_period, spei3)
#' @noRd
blend_global_drought_spei <- function(spei_long, year) {
  w <- heat_blend_weights(year, unique(spei_long$centre_year))
  spei_long |>
    dplyr::inner_join(w, by = "centre_year") |>
    dplyr::group_by(.data$asset, .data$season, .data$return_period) |>
    dplyr::summarise(
      spei3 = if (any(is.na(.data$spei3))) NA_real_ else sum(.data$weight * .data$spei3),
      .groups = "drop"
    )
}

# ---------------------------------------------------------------------------
# Crop resolution
# ---------------------------------------------------------------------------

#' Normalise free-text crop input to a supported crop key (internal)
#'
#' @description Maize = Corn. "Maize 2" / second maize / safrinha -> `maize2`;
#'   "Winter wheat" -> `winter_wheat`; Soy / Soya beans / Soybean(s) -> `soybean`.
#'   Anything else (Rice, Coffee, Banana, blank, ...) -> NA (unknown).
#' @return character vector of crop keys (NA = unknown)
#' @noRd
normalize_drought_crop <- function(x) {
  k <- tolower(trimws(enc2utf8(as.character(x))))
  k <- gsub("[_]+", " ", k)
  k <- gsub("\\s+", " ", k)
  out <- rep(NA_character_, length(k))
  ok <- !is.na(k) & nzchar(k)

  is_maize <- ok & grepl("\\b(maize|corn|milho)\\b", k)
  is_second <- grepl("(\\b2\\b|second|2nd|safrinha|segunda)", k)
  out[is_maize & is_second] <- "maize2"
  out[is_maize & !is_second] <- "maize"

  is_soy <- ok & is.na(out) & grepl("\\b(soy|soya|soja|soybeans?|soyabeans?)\\b", k)
  out[is_soy] <- "soybean"

  is_wheat <- ok & is.na(out) & grepl("\\b(wheat|trigo)\\b", k)
  is_winter <- grepl("winter", k)
  out[is_wheat & is_winter] <- "winter_wheat"
  out[is_wheat & !is_winter] <- "wheat"
  out
}

#' Load the country -> most produced crop (wheat / soy / maize) proxy table (internal)
#' @return tibble(country_name, iso2, iso3, proxy_crop_key, proxy_crop_input)
#' @noRd
load_drought_crop_proxy_table <- function(mappings_dir, mapping_file = "Most used Crop Country List_WSM.xlsx") {
  path <- file.path(mappings_dir, mapping_file)
  if (!file.exists(path)) {
    stop("Drought crop proxy table not found: ", path)
  }
  raw <- readxl::read_excel(path, col_types = "text", na = c("", "NA"))
  names(raw) <- to_snake_case(names(raw))
  required <- c("country", "iso2", "iso3", "top_crop")
  missing <- setdiff(required, names(raw))
  if (length(missing) > 0) {
    stop("Drought crop proxy table is missing column(s): ", paste(missing, collapse = ", "), " (", path, ")")
  }
  tibble::tibble(
    country_name = trimws(enc2utf8(raw$country)),
    iso2 = toupper(trimws(raw$iso2)),
    iso3 = toupper(trimws(raw$iso3)),
    proxy_crop_input = trimws(raw$top_crop),
    proxy_crop_key = normalize_drought_crop(raw$top_crop)
  )
}

#' Resolve the crop of each asset (internal)
#'
#' @description (1) the asset's own crop (`asset_subtype`) when it names maize/corn, maize 2,
#'   soybean, wheat or winter wheat; otherwise (2) the most produced of wheat/soy/maize in the
#'   asset's country (matched by ISO2, ISO3 or name; "UK" = GB); otherwise (3) maize.
#' @return tibble(crop_input, crop_key, crop, crop_source, damage_crop, calendar_file)
#' @noRd
resolve_drought_crop <- function(crop_values, country_values, proxy_table) {
  n <- length(crop_values)
  key <- normalize_drought_crop(crop_values)
  source <- ifelse(is.na(key), NA_character_, "input")

  ckey <- toupper(trimws(enc2utf8(as.character(country_values))))
  ckey[ckey == "UK"] <- "GB"
  cname <- tolower(trimws(enc2utf8(as.character(country_values))))
  need <- which(is.na(key))
  for (i in need) {
    j <- NA_integer_
    if (!is.na(ckey[[i]]) && nzchar(ckey[[i]])) {
      j <- match(ckey[[i]], proxy_table$iso2)
      if (is.na(j)) j <- match(ckey[[i]], proxy_table$iso3)
      if (is.na(j)) j <- match(cname[[i]], tolower(proxy_table$country_name))
    }
    if (!is.na(j) && !is.na(proxy_table$proxy_crop_key[[j]])) {
      key[[i]] <- proxy_table$proxy_crop_key[[j]]
      source[[i]] <- "country_proxy"
    } else {
      key[[i]] <- DROUGHT_DEFAULT_CROP
      source[[i]] <- "default"
    }
  }

  crops <- drought_crop_table()
  idx <- match(key, crops$crop_key)
  tibble::tibble(
    crop_input = as.character(crop_values),
    crop_key = key,
    crop = crops$crop_label[idx],
    crop_source = source,
    damage_crop = crops$damage_crop[idx],
    calendar_file = crops$calendar_file[idx]
  )
}

# ---------------------------------------------------------------------------
# Crop calendars
# ---------------------------------------------------------------------------

.drought_calendar_cache <- new.env(parent = emptyenv())

#' Load one crop calendar: growing-season flags per 0.5 deg cell (internal, cached)
#'
#' @return list(valid = tibble(lon, lat, DJF, MAM, JJA, SON) of cells with at least one
#'   growing season, lon, lat (axis vectors), flags = array lon x lat x season (logical),
#'   has_cal = matrix lon x lat (logical))
#' @noRd
load_drought_crop_calendar <- function(path) {
  key <- normalizePath(path, mustWork = FALSE)
  info <- file.info(path)
  stamp <- paste(key, info$size, as.numeric(info$mtime))
  if (!is.null(.drought_calendar_cache[[stamp]])) {
    return(.drought_calendar_cache[[stamp]])
  }
  if (!file.exists(path)) {
    stop("Crop calendar not found: ", path)
  }
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  dims <- vapply(nc$var[["grow.season"]]$dim, function(d) d$name, character(1))
  flags <- ncdf4::ncvar_get(nc, "grow.season", collapse_degen = FALSE)
  # Arrange as lon x lat x season whatever the on-disk order.
  perm <- match(c("longitude", "latitude", "season"), dims)
  if (anyNA(perm)) {
    stop("Crop calendar 'grow.season' must have longitude, latitude and season dimensions: ", path)
  }
  flags <- aperm(flags, perm)
  season_codes <- as.integer(ncdf4::ncvar_get(nc, "season"))
  lon <- as.numeric(ncdf4::ncvar_get(nc, "longitude"))
  lat <- as.numeric(ncdf4::ncvar_get(nc, "latitude"))

  flags <- !is.na(flags) & flags == 1
  # season codes: 1 = DJF, 2 = MAM, 3 = JJA, 4 = SON
  flags <- flags[, , order(season_codes), drop = FALSE]
  has_cal <- apply(flags, c(1, 2), any)

  idx <- which(has_cal, arr.ind = TRUE)
  valid <- tibble::tibble(
    lon = lon[idx[, 1]],
    lat = lat[idx[, 2]],
    DJF = flags[cbind(idx, 1L)],
    MAM = flags[cbind(idx, 2L)],
    JJA = flags[cbind(idx, 3L)],
    SON = flags[cbind(idx, 4L)]
  )
  out <- list(valid = valid, lon = lon, lat = lat, flags = flags, has_cal = has_cal)
  assign(stamp, out, envir = .drought_calendar_cache)
  out
}

#' Great-circle distance in km (internal)
#' @noRd
haversine_km <- function(lon1, lat1, lon2, lat2) {
  rad <- pi / 180
  dlat <- (lat2 - lat1) * rad
  dlon <- (lon2 - lon1) * rad
  a <- sin(dlat / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin(dlon / 2)^2
  2 * 6371.0088 * asin(pmin(1, sqrt(a)))
}

#' Growing seasons of a crop at asset locations (internal)
#'
#' @description The asset's own calendar cell is used when it has a growing period
#'   for the crop; otherwise the nearest cell that has one (great-circle distance),
#'   reported with its distance.
#' @return tibble(calendar_match, calendar_distance_km, calendar_lon, calendar_lat,
#'   gs_DJF, gs_MAM, gs_JJA, gs_SON, growing_seasons)
#' @noRd
lookup_drought_growing_seasons <- function(lon, lat, calendar) {
  n <- length(lon)
  out <- tibble::tibble(
    calendar_match = rep("no_coordinates", n),
    calendar_distance_km = rep(NA_real_, n),
    calendar_lon = rep(NA_real_, n),
    calendar_lat = rep(NA_real_, n),
    gs_DJF = rep(NA, n), gs_MAM = rep(NA, n), gs_JJA = rep(NA, n), gs_SON = rep(NA, n)
  )
  half_x <- if (length(calendar$lon) > 1) min(abs(diff(calendar$lon))) / 2 else Inf
  half_y <- if (length(calendar$lat) > 1) min(abs(diff(calendar$lat))) / 2 else Inf
  for (i in seq_len(n)) {
    if (is.na(lon[[i]]) || is.na(lat[[i]])) next
    ix <- which.min(abs(calendar$lon - lon[[i]]))
    iy <- which.min(abs(calendar$lat - lat[[i]]))
    # the asset lies in that grid cell (not merely closest to the grid's edge)
    in_cell <- abs(calendar$lon[[ix]] - lon[[i]]) <= half_x + 1e-9 && abs(calendar$lat[[iy]] - lat[[i]]) <= half_y + 1e-9
    if (in_cell && isTRUE(calendar$has_cal[ix, iy])) {
      f <- calendar$flags[ix, iy, ]
      out$calendar_match[[i]] <- "cell"
      out$calendar_distance_km[[i]] <- 0
      out$calendar_lon[[i]] <- calendar$lon[[ix]]
      out$calendar_lat[[i]] <- calendar$lat[[iy]]
    } else if (nrow(calendar$valid) > 0) {
      d <- haversine_km(lon[[i]], lat[[i]], calendar$valid$lon, calendar$valid$lat)
      j <- which.min(d)
      f <- unlist(calendar$valid[j, DROUGHT_SEASONS], use.names = FALSE)
      out$calendar_match[[i]] <- "nearest_cell"
      out$calendar_distance_km[[i]] <- d[[j]]
      out$calendar_lon[[i]] <- calendar$valid$lon[[j]]
      out$calendar_lat[[i]] <- calendar$valid$lat[[j]]
    } else {
      out$calendar_match[[i]] <- "none"
      next
    }
    out$gs_DJF[[i]] <- f[[1]]
    out$gs_MAM[[i]] <- f[[2]]
    out$gs_JJA[[i]] <- f[[3]]
    out$gs_SON[[i]] <- f[[4]]
  }
  gs <- as.matrix(out[, paste0("gs_", DROUGHT_SEASONS)])
  out$growing_seasons <- apply(gs, 1, function(r) {
    if (all(is.na(r))) NA_character_ else paste(DROUGHT_SEASONS[which(r)], collapse = "|")
  })
  out
}

# ---------------------------------------------------------------------------
# Damage function
# ---------------------------------------------------------------------------

#' Load the global drought damage function (internal)
#'
#' @description Columns `SPEI3`, `damage factor`, `crop`, `off_window`. Small negative
#'   damage factors (yield gains for mildly dry conditions in the source curve) are
#'   clamped to 0: drought never raises revenue in the model.
#' @return tibble(crop, spei3, damage_factor, off_window)
#' @noRd
load_drought_damage_function <- function(mappings_dir, mapping_file = "drought_damage_function_global.xlsx") {
  path <- file.path(mappings_dir, mapping_file)
  if (!file.exists(path)) {
    stop("Drought damage function not found: ", path)
  }
  raw <- readxl::read_excel(path)
  names(raw) <- to_snake_case(names(raw))
  required <- c("spei3", "damage_factor", "crop", "off_window")
  missing <- setdiff(required, names(raw))
  if (length(missing) > 0) {
    stop("Drought damage function is missing column(s): ", paste(missing, collapse = ", "), " (", path, ")")
  }
  out <- tibble::tibble(
    crop = trimws(as.character(raw$crop)),
    spei3 = round(as.numeric(raw$spei3), 6),
    damage_factor = pmax(0, as.numeric(raw$damage_factor)),
    off_window = as.numeric(raw$off_window)
  ) |>
    dplyr::filter(!is.na(.data$crop), !is.na(.data$spei3), !is.na(.data$damage_factor))
  needed <- unique(drought_crop_table()$damage_crop)
  absent <- setdiff(needed, unique(out$crop))
  if (length(absent) > 0) {
    stop("Drought damage function has no rows for crop(s): ", paste(absent, collapse = ", "), " (", path, ")")
  }
  if (anyNA(out$off_window) || any(out$off_window < 0 | out$off_window > 1)) {
    stop("Drought damage function: off_window must be a share in [0, 1] (", path, ")")
  }
  out
}

#' In-window damage factor for SPEI-3 values (closest SPEI step of the crop's curve) (internal)
#'
#' @description Same "closest" matching as Brazil's drought table: values below the
#'   curve's driest step use that step (-3), values at or above 0 (normal / wet) give 0.
#' @return tibble(spei3_matched, damage_factor_in_window, off_window); NA where spei3 is NA
#' @noRd
lookup_drought_damage <- function(spei3, damage_crop, damage_table) {
  n <- length(spei3)
  out <- tibble::tibble(
    spei3_matched = rep(NA_real_, n),
    damage_factor_in_window = rep(NA_real_, n),
    off_window = rep(NA_real_, n)
  )
  for (cr in unique(damage_crop[!is.na(damage_crop)])) {
    tab <- damage_table[damage_table$crop == cr, , drop = FALSE]
    tab <- tab[order(tab$spei3), , drop = FALSE]
    if (nrow(tab) == 0) {
      stop("lookup_drought_damage: no damage curve for crop '", cr, "'")
    }
    rows <- which(damage_crop == cr & !is.na(spei3))
    if (length(rows) > 0) {
      # closest step via the midpoints between steps; an exact tie goes to the drier step
      mids <- (utils::head(tab$spei3, -1) + utils::tail(tab$spei3, -1)) / 2
      j <- findInterval(spei3[rows], mids, left.open = TRUE) + 1L
      out$spei3_matched[rows] <- tab$spei3[j]
      out$damage_factor_in_window[rows] <- tab$damage_factor[j]
      out$off_window[rows] <- tab$off_window[j]
    }
    # off_window is a crop constant: keep it for rows without a SPEI value too
    no_val <- which(damage_crop == cr & is.na(spei3))
    if (length(no_val) > 0) out$off_window[no_val] <- tab$off_window[[1]]
  }
  out
}

#' Effective damage: in-window factor, or in-window factor x off_window outside the growing period (internal)
#' @noRd
drought_effective_damage <- function(damage_in_window, in_growing_period, off_window) {
  ifelse(isTRUE_vec(in_growing_period), damage_in_window, damage_in_window * off_window)
}

#' Vectorised isTRUE (internal)
#' @noRd
isTRUE_vec <- function(x) !is.na(x) & as.logical(x)

#' TRUE for agriculture assets (case-insensitive) (internal)
#' @noRd
is_agriculture_asset <- function(asset_category) {
  !is.na(asset_category) & tolower(trimws(as.character(asset_category))) == "agriculture"
}

# ---------------------------------------------------------------------------
# Asset attributes + deterministic event rows
# ---------------------------------------------------------------------------

#' Per-asset drought attributes: crop and growing seasons (internal)
#'
#' @param assets assets (already filtered to portfolio companies)
#' @param hazards_dir `.../hazards/config_global`
#' @param hazard_config the global drought config
#' @return tibble(asset, is_agriculture, crop_input, crop_key, crop, crop_source, damage_crop,
#'   calendar_crop, calendar_match, calendar_distance_km, calendar_lon, calendar_lat,
#'   gs_DJF, gs_MAM, gs_JJA, gs_SON, growing_seasons)
#' @noRd
build_global_drought_asset_attributes <- function(assets, hazards_dir, hazard_config) {
  mappings_dir <- file.path(dirname(hazards_dir), "mappings")
  calendar_dir <- global_drought_calendar_dir(hazards_dir, hazard_config)
  proxy <- load_drought_crop_proxy_table(
    mappings_dir, global_drought_mapping_file(hazard_config, "crop_proxy", "Most used Crop Country List_WSM.xlsx")
  )

  crop_col <- intersect(c("asset_subtype", "crop"), names(assets))
  crop_values <- if (length(crop_col) > 0) assets[[crop_col[[1]]]] else rep(NA_character_, nrow(assets))
  country_values <- if ("country" %in% names(assets)) assets$country else rep(NA_character_, nrow(assets))
  category <- if ("asset_category" %in% names(assets)) assets$asset_category else rep(NA_character_, nrow(assets))

  crops <- resolve_drought_crop(crop_values, country_values, proxy)
  lon <- suppressWarnings(as.numeric(assets$longitude))
  lat <- suppressWarnings(as.numeric(assets$latitude))

  cal <- tibble::tibble(
    calendar_match = rep(NA_character_, nrow(assets)),
    calendar_distance_km = NA_real_, calendar_lon = NA_real_, calendar_lat = NA_real_,
    gs_DJF = NA, gs_MAM = NA, gs_JJA = NA, gs_SON = NA, growing_seasons = NA_character_
  )
  is_ag <- is_agriculture_asset(category)
  for (f in unique(crops$calendar_file[is_ag])) {
    rows <- which(is_ag & crops$calendar_file == f)
    calendar <- load_drought_crop_calendar(file.path(calendar_dir, f))
    cal[rows, ] <- lookup_drought_growing_seasons(lon[rows], lat[rows], calendar)
  }

  out <- dplyr::bind_cols(
    tibble::tibble(asset = as.character(assets$asset), is_agriculture = is_ag),
    crops |> dplyr::select(-"calendar_file"),
    tibble::tibble(calendar_crop = sub("\\.crop\\.calendar\\.nc$", "", crops$calendar_file)),
    cal
  )
  # Crop fields are meaningful for agriculture assets only
  for (col in c("crop_key", "crop", "crop_source", "damage_crop", "calendar_crop")) {
    out[[col]][!is_ag] <- NA_character_
  }
  out
}

#' Warnings describing fallbacks applied while building drought attributes (internal)
#' @noRd
global_drought_attribute_warnings <- function(attrs, spei_missing_assets = character(0)) {
  msgs <- character(0)
  ag <- attrs[attrs$is_agriculture, , drop = FALSE]
  non_ag <- attrs$asset[!attrs$is_agriculture]
  if (length(non_ag) > 0) {
    msgs <- c(msgs, paste0(
      "[drought] Drought only affects agriculture assets; no drought shock for non-agriculture asset(s) ",
      paste(non_ag, collapse = ", "), "."
    ))
  }
  prox <- ag[ag$crop_source == "country_proxy", , drop = FALSE]
  if (nrow(prox) > 0) {
    msgs <- c(msgs, paste0(
      "[drought] Crop not given or not covered (maize, soybean, wheat) for asset(s) ",
      paste0(prox$asset, " (", ifelse(is.na(prox$crop_input), "blank", prox$crop_input), " -> ", prox$crop, ")", collapse = ", "),
      "; used the country's most produced crop."
    ))
  }
  dflt <- ag[ag$crop_source == "default", , drop = FALSE]
  if (nrow(dflt) > 0) {
    msgs <- c(msgs, paste0(
      "[drought] Crop unknown and no country proxy crop for asset(s) ", paste(dflt$asset, collapse = ", "),
      "; assumed Maize."
    ))
  }
  near <- ag[!is.na(ag$calendar_match) & ag$calendar_match == "nearest_cell", , drop = FALSE]
  if (nrow(near) > 0) {
    msgs <- c(msgs, paste0(
      "[drought] No crop calendar at the location of asset(s) ",
      paste0(near$asset, " (", near$crop, ", ", round(near$calendar_distance_km), " km)", collapse = ", "),
      "; used the nearest calendar cell for that crop."
    ))
  }
  nocal <- ag[!is.na(ag$calendar_match) & ag$calendar_match == "none", , drop = FALSE]
  if (nrow(nocal) > 0) {
    msgs <- c(msgs, paste0(
      "[drought] No crop calendar available for asset(s) ", paste(nocal$asset, collapse = ", "),
      "; treated as outside the growing period."
    ))
  }
  if (length(spei_missing_assets) > 0) {
    msgs <- c(msgs, paste0(
      "[drought] No drought-map value (missing coordinates or location outside the map) for asset(s) ",
      paste(spei_missing_assets, collapse = ", "), "; no drought shock applied."
    ))
  }
  msgs
}

#' Build deterministic Drought event rows (one per asset x event) for the shock engine (internal)
#'
#' @description Rows carry `damage_factor` (the effective factor, used by the config's revenue
#'   equation) plus display columns (SPEI-3, crop, growing seasons, off-window share). They are
#'   bound to the other hazards' rows in `compute_risk()` and go through the unmodified
#'   `compute_shock_trajectories()`.
#' @param events_drought drought event rows (event_id, hazard_type, hazard_name, scenario_name,
#'   season, return_period, event_year, statistic)
#' @return list(rows = tibble, warnings = character)
#' @noRd
build_global_drought_event_factors <- function(assets, events_drought, hazard_configs, hazards_dir,
                                               spatial_separation_data = NULL, base_dir = NULL) {
  if (is.null(events_drought) || nrow(events_drought) == 0) {
    return(list(rows = tibble::tibble(), warnings = character(0)))
  }
  hazard_config <- hazard_configs[[as.character(events_drought$hazard_type[[1]])]]
  mappings_dir <- file.path(dirname(hazards_dir), "mappings")
  damage_table <- load_drought_damage_function(
    mappings_dir, global_drought_mapping_file(hazard_config, "damage_function", "drought_damage_function_global.xlsx")
  )
  attrs <- build_global_drought_asset_attributes(assets, hazards_dir, hazard_config)

  meta_cols <- intersect(
    c("asset", "company", "latitude", "longitude", "country", "asset_category",
      "asset_subtype", "share_of_economic_activity"),
    names(assets)
  )
  asset_meta <- assets |>
    dplyr::select(dplyr::all_of(meta_cols)) |>
    dplyr::mutate(asset = as.character(.data$asset)) |>
    dplyr::distinct(.data$asset, .keep_all = TRUE) |>
    dplyr::left_join(attrs, by = "asset")

  rows <- list()
  missing_assets <- character(0)
  for (i in seq_len(nrow(events_drought))) {
    ev <- events_drought[i, ]
    hazard_config <- hazard_configs[[as.character(ev$hazard_type)]]
    maps_dir <- global_drought_maps_dir(hazards_dir, hazard_config)
    season <- toupper(trimws(as.character(ev$season)))
    if (is.na(season) || !season %in% DROUGHT_SEASONS) {
      stop(
        "Global drought event ", ev$event_id, " needs a season (one of ",
        paste(DROUGHT_SEASONS, collapse = ", "), "); got '", ev$season, "'"
      )
    }
    statistic <- if ("statistic" %in% names(ev) && !is.na(ev$statistic) && nzchar(as.character(ev$statistic))) {
      as.character(ev$statistic)
    } else {
      "median"
    }
    year <- as.numeric(ev$event_year)
    rp <- as.numeric(ev$return_period)

    ag_pts <- asset_meta[asset_meta$is_agriculture, , drop = FALSE]
    blended <- tibble::tibble(asset = character(), spei3 = numeric())
    w <- tibble::tibble(centre_year = numeric(), weight = numeric())
    if (nrow(ag_pts) > 0) {
      spei_long <- extract_global_drought_spei(
        ag_pts, maps_dir, ssp = as.character(ev$scenario_name), seasons = season,
        statistic = statistic, return_periods = rp
      )
      blended <- blend_global_drought_spei(spei_long, year) |> dplyr::select("asset", "spei3")
      w <- heat_blend_weights(year, unique(spei_long$centre_year))
      missing_assets <- union(missing_assets, blended$asset[is.na(blended$spei3)])
    }

    r <- asset_meta |>
      dplyr::left_join(blended, by = "asset")
    dmg <- lookup_drought_damage(r$spei3, ifelse(r$is_agriculture, r$damage_crop, NA_character_), damage_table)
    in_gp <- r[[paste0("gs_", season)]]
    in_gp <- ifelse(r$is_agriculture, as.logical(in_gp), NA)

    rows[[i]] <- dplyr::bind_cols(r, dmg) |>
      dplyr::mutate(
        in_growing_period = in_gp,
        damage_factor = dplyr::case_when(
          !.data$is_agriculture ~ 0,
          is.na(.data$damage_factor_in_window) ~ 0,
          TRUE ~ drought_effective_damage(.data$damage_factor_in_window, .data$in_growing_period, .data$off_window)
        ),
        drought_status = dplyr::case_when(
          !.data$is_agriculture ~ "not applicable (non-agriculture asset)",
          is.na(.data$spei3) ~ "no drought-map value at location",
          isTRUE_vec(.data$in_growing_period) ~ "season in growing period",
          TRUE ~ "season outside growing period (off-window share)"
        ),
        season = season,
        event_id = as.character(ev$event_id),
        hazard_type = as.character(ev$hazard_type),
        hazard_name = as.character(ev$hazard_name),
        scenario_name = as.character(ev$scenario_name),
        hazard_return_period = rp,
        event_year = as.integer(year),
        statistic = statistic,
        drought_map_weights = if (nrow(w) > 0) paste0(w$centre_year, ":", round(w$weight, 4), collapse = "; ") else NA_character_,
        spatial_included = TRUE,
        spatial_multiplier = 1,
        spatial_exposure_status = NA_character_,
        matching_method = "coordinates"
      )
  }

  rows <- dplyr::bind_rows(rows)
  rows <- apply_global_adm_spatial_selection(
    rows, events_drought, hazard_configs, spatial_separation_data, base_dir,
    value_cols = c("spei3", "spei3_matched", "damage_factor_in_window", "damage_factor"),
    label = "drought"
  )

  list(
    rows = rows,
    warnings = global_drought_attribute_warnings(attrs, missing_assets)
  )
}

# ---------------------------------------------------------------------------
# Probabilistic (AAL) drought
# ---------------------------------------------------------------------------

#' Linear AAL weights for a fixed set of return periods (internal)
#'
#' @description `calculate_aal()` is linear in the losses, so AAL = sum_k w_k * L_k with
#'   w_k = calculate_aal(rps, e_k). Same boundary rules (0 loss at RP 1, flat beyond the
#'   largest RP); lets the drought AAL be evaluated for many assets x seasons x years at once.
#' @noRd
aal_weights_for_rps <- function(rps) {
  rps <- sort(unique(as.numeric(rps)))
  vapply(seq_along(rps), function(k) {
    e <- numeric(length(rps))
    e[[k]] <- 1
    calculate_aal(rps, e)
  }, numeric(1))
}

#' Drought AAL for every year of the projection horizon (internal)
#'
#' @description For each year the SPEI-3 maps of all four seasons are blended to that
#'   year's climate. Per asset, season and return period the yield-loss rate is the
#'   damage function at that SPEI-3, in full when the season is in the crop's growing
#'   period and times the off-window share otherwise. Each season's loss curve is
#'   integrated into an AAL (`calculate_aal()`: zero loss at RP 1, flat beyond the
#'   largest RP) and the annual drought AAL is the mean over the four seasons. Regions
#'   whose growing period spans more seasons therefore carry a higher AAL.
#' @param event one probabilistic Drought event (scenario_name, statistic)
#' @param years integer vector of projection years
#' @return list(summary = tibble(asset, aal_drought_revenue_rate = mean over years),
#'   yearly = tibble(asset, year, aal_drought_revenue_rate),
#'   detail = tibble(asset, return_period, channel, loss, aep) (mean over seasons and years),
#'   factors_by_rp = tibble (per asset x return period diagnostics), warnings = character)
#' @noRd
compute_global_drought_aal <- function(assets, event, hazard_configs, hazards_dir, years) {
  hazard_type <- as.character(event$hazard_type[[1]])
  hazard_config <- hazard_configs[[hazard_type]]
  maps_dir <- global_drought_maps_dir(hazards_dir, hazard_config)
  mappings_dir <- file.path(dirname(hazards_dir), "mappings")
  damage_table <- load_drought_damage_function(
    mappings_dir, global_drought_mapping_file(hazard_config, "damage_function", "drought_damage_function_global.xlsx")
  )
  attrs <- build_global_drought_asset_attributes(assets, hazards_dir, hazard_config)

  statistic <- if ("statistic" %in% names(event) && !is.na(event$statistic[[1]]) && nzchar(as.character(event$statistic[[1]]))) {
    as.character(event$statistic[[1]])
  } else {
    "median"
  }

  asset_pts <- assets |>
    dplyr::mutate(asset = as.character(.data$asset)) |>
    dplyr::distinct(.data$asset, .keep_all = TRUE)
  all_assets <- asset_pts$asset
  ag_attrs <- attrs[attrs$is_agriculture, , drop = FALSE]
  years <- sort(unique(as.integer(years)))

  empty_yearly <- tidyr::crossing(asset = all_assets, year = years) |>
    dplyr::mutate(aal_drought_revenue_rate = 0)

  if (nrow(ag_attrs) == 0) {
    return(list(
      summary = tibble::tibble(asset = all_assets, aal_drought_revenue_rate = 0),
      yearly = empty_yearly,
      detail = tibble::tibble(asset = character(), return_period = numeric(), channel = character(), loss = numeric(), aep = numeric()),
      factors_by_rp = tibble::tibble(),
      warnings = global_drought_attribute_warnings(attrs)
    ))
  }

  ag_pts <- asset_pts[asset_pts$asset %in% ag_attrs$asset, , drop = FALSE]
  spei_long <- extract_global_drought_spei(
    ag_pts, maps_dir, ssp = as.character(event$scenario_name[[1]]),
    seasons = DROUGHT_SEASONS, statistic = statistic
  )
  missing_assets <- unique(spei_long$asset[is.na(spei_long$spei3)])
  rps <- sort(unique(spei_long$return_period))
  w_rp <- aal_weights_for_rps(rps)

  season_flags <- ag_attrs |>
    dplyr::select("asset", "damage_crop", dplyr::all_of(paste0("gs_", DROUGHT_SEASONS))) |>
    tidyr::pivot_longer(dplyr::all_of(paste0("gs_", DROUGHT_SEASONS)), names_to = "season", values_to = "in_growing_period") |>
    dplyr::mutate(season = sub("^gs_", "", .data$season), in_growing_period = isTRUE_vec(.data$in_growing_period))

  yearly <- vector("list", length(years))
  per_rp <- vector("list", length(years))
  for (k in seq_along(years)) {
    b <- blend_global_drought_spei(spei_long, years[[k]]) |>
      dplyr::left_join(season_flags, by = c("asset", "season"))
    dmg <- lookup_drought_damage(b$spei3, b$damage_crop, damage_table)
    b$loss <- dplyr::coalesce(
      drought_effective_damage(dmg$damage_factor_in_window, b$in_growing_period, dmg$off_window),
      0
    )
    # AAL per asset x season (weights over the fixed RP set), then mean over seasons
    b$w <- w_rp[match(b$return_period, rps)]
    season_aal <- b |>
      dplyr::group_by(.data$asset, .data$season) |>
      dplyr::summarise(aal = sum(.data$w * .data$loss), .groups = "drop")
    yearly[[k]] <- season_aal |>
      dplyr::group_by(.data$asset) |>
      dplyr::summarise(aal_drought_revenue_rate = mean(.data$aal), .groups = "drop") |>
      dplyr::mutate(year = years[[k]])
    per_rp[[k]] <- b |>
      dplyr::select("asset", "season", "return_period", "spei3", "loss") |>
      dplyr::mutate(year = years[[k]])
  }
  yearly <- dplyr::bind_rows(yearly)
  per_rp <- dplyr::bind_rows(per_rp)

  # Non-agriculture assets: zero drought AAL in every year
  yearly <- empty_yearly |>
    dplyr::select("asset", "year") |>
    dplyr::left_join(yearly, by = c("asset", "year")) |>
    dplyr::mutate(aal_drought_revenue_rate = dplyr::coalesce(.data$aal_drought_revenue_rate, 0)) |>
    dplyr::select("asset", "year", "aal_drought_revenue_rate")

  summary <- yearly |>
    dplyr::group_by(.data$asset) |>
    dplyr::summarise(aal_drought_revenue_rate = mean(.data$aal_drought_revenue_rate), .groups = "drop")

  detail <- per_rp |>
    dplyr::group_by(.data$asset, .data$return_period) |>
    dplyr::summarise(loss = mean(.data$loss), .groups = "drop") |>
    dplyr::mutate(channel = "aal_drought_revenue_rate", aep = 1 / .data$return_period) |>
    dplyr::select("asset", "return_period", "channel", "loss", "aep") |>
    dplyr::arrange(.data$asset, .data$return_period)

  spei_wide <- per_rp |>
    dplyr::group_by(.data$asset, .data$return_period, .data$season) |>
    dplyr::summarise(spei3 = mean(.data$spei3), .groups = "drop") |>
    dplyr::mutate(season = paste0("spei3_", .data$season)) |>
    tidyr::pivot_wider(names_from = "season", values_from = "spei3")
  loss_rp <- per_rp |>
    dplyr::group_by(.data$asset, .data$return_period) |>
    dplyr::summarise(drought_loss_rate = mean(.data$loss), .groups = "drop")

  meta_cols <- intersect(
    c("asset", "company", "latitude", "longitude", "country", "asset_category", "asset_subtype",
      "share_of_economic_activity"),
    names(ag_pts)
  )
  factors_by_rp <- ag_pts |>
    dplyr::select(dplyr::all_of(meta_cols)) |>
    dplyr::left_join(ag_attrs, by = "asset") |>
    dplyr::inner_join(loss_rp, by = "asset") |>
    dplyr::left_join(spei_wide, by = c("asset", "return_period")) |>
    dplyr::mutate(
      hazard_type = hazard_type,
      scenario_name = as.character(event$scenario_name[[1]]),
      statistic = statistic,
      off_window = damage_table$off_window[match(.data$damage_crop, damage_table$crop)]
    ) |>
    dplyr::arrange(.data$asset, .data$return_period)

  list(
    summary = summary,
    yearly = yearly,
    detail = detail,
    factors_by_rp = factors_by_rp,
    warnings = global_drought_attribute_warnings(attrs, missing_assets)
  )
}

#' Add the revenue adjustment to the yearly drought AAL (probabilistic) (internal)
#'
#' @description For each year `baseline_revenue x (-AAL rate)` (negative = revenue lost);
#'   the summary carries its mean over the horizon.
#' @noRd
add_drought_aal_revenue_adjustment <- function(drought_aal, yearly_baseline) {
  base <- yearly_baseline |>
    dplyr::transmute(asset = as.character(.data$asset), year = as.integer(.data$year), baseline_revenue = .data$revenue) |>
    dplyr::distinct(.data$asset, .data$year, .keep_all = TRUE)
  drought_aal$yearly <- drought_aal$yearly |>
    dplyr::mutate(asset = as.character(.data$asset), year = as.integer(.data$year)) |>
    dplyr::left_join(base, by = c("asset", "year")) |>
    dplyr::mutate(drought_revenue_adjustment = -.data$baseline_revenue * .data$aal_drought_revenue_rate)
  avg <- drought_aal$yearly |>
    dplyr::group_by(.data$asset) |>
    dplyr::summarise(avg_annual_drought_revenue_adjustment = mean(.data$drought_revenue_adjustment), .groups = "drop")
  drought_aal$summary <- drought_aal$summary |>
    dplyr::left_join(avg, by = "asset")
  drought_aal
}

#' Revenue adjustment for deterministic drought rows: -baseline revenue (event year) x damage factor (internal)
#' @noRd
add_drought_revenue_adjustment <- function(assets_factors, yearly_baseline, drought_types) {
  if (is.null(assets_factors) || nrow(assets_factors) == 0 || !"drought_status" %in% names(assets_factors)) {
    return(assets_factors)
  }
  base <- yearly_baseline |>
    dplyr::transmute(
      asset = as.character(.data$asset), event_year = as.integer(.data$year),
      drought_baseline_revenue = .data$revenue
    ) |>
    dplyr::distinct(.data$asset, .data$event_year, .keep_all = TRUE)
  is_drought <- !is.na(assets_factors$hazard_type) & assets_factors$hazard_type %in% drought_types &
    !is.na(assets_factors$drought_status)
  out <- assets_factors |>
    dplyr::mutate(asset = as.character(.data$asset), event_year = as.integer(.data$event_year)) |>
    dplyr::left_join(base, by = c("asset", "event_year"))
  out$drought_revenue_adjustment <- ifelse(
    is_drought & !is.na(out$damage_factor),
    -out$drought_baseline_revenue * out$damage_factor,
    NA_real_
  )
  out$drought_baseline_revenue <- NULL
  out
}

#' Display labels for drought crops that were assumed rather than given (internal)
#'
#' @description For assets whose crop came from the country proxy or the maize default,
#'   `crop` and `calendar_crop` are shown as e.g. "NA (assumed Soybean)" /
#'   "NA (assumed Soybeans)", so the table makes clear that the input crop (in `crop_input`,
#'   e.g. "Banana") is not covered and another crop's curve and calendar were used.
#'   `crop_source` is spelled out. Display only: results objects keep the plain values.
#' @noRd
label_assumed_drought_crop <- function(df) {
  if (is.null(df) || !"crop_source" %in% names(df)) {
    return(df)
  }
  assumed <- !is.na(df$crop_source) & df$crop_source %in% c("country_proxy", "default")
  for (col in intersect(c("crop", "calendar_crop"), names(df))) {
    v <- as.character(df[[col]])
    df[[col]] <- ifelse(assumed & !is.na(v), paste0("NA (assumed ", v, ")"), v)
  }
  df$crop_source <- dplyr::case_when(
    df$crop_source == "input" ~ "input",
    df$crop_source == "country_proxy" ~ "assumed: most produced of maize/soybean/wheat in the country",
    df$crop_source == "default" ~ "assumed: maize (no country proxy)",
    TRUE ~ as.character(df$crop_source)
  )
  df
}
