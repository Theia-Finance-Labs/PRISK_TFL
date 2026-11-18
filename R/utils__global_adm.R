# Global administrative-region spatial separation (Global mode, Heat, deterministic).
#
# Shapefiles (geoBoundaries CGAZ, WGS84) under
# `{base_dir}/spatial_separation/global_adm_regions/geoBoundariesCGAZ_ADM{0,1,2}/`:
#   ADM0 = countries (218), ADM1 = first-level subdivisions (3,224), ADM2 = second-level
#   subdivisions (49,349). Fields: shapeGroup (ISO3 country code), shapeName, shapeID
#   (unique id; ADM0 has none, its ISO3 is the unique key).
#
# The user picks a granularity (country / ADM1 / ADM2), then countries, then -- for ADM1/ADM2 --
# the regions inside those countries (multi-select at every step). Loading is lazy so app
# start-up stays instant and the 49k-polygon ADM2 file is never read as a whole:
#   1. attributes only (the .dbf files, ~2 s) populate the dropdowns;
#   2. polygons are read per country (OGR attribute filter, ~0.3 s for Brazil's 5,570 ADM2),
#      repaired if needed and cached for the session.

#' Levels offered for global administrative separation (internal)
#' @noRd
GLOBAL_ADM_LEVELS <- c(adm0 = 0L, adm1 = 1L, adm2 = 2L)

# Session cache of repaired polygons per (level, country), keyed by file path + mtime + size.
.global_adm_cache <- new.env(parent = emptyenv())

# Session cache of ADM2 -> parent ADM1 names per country (used only to tell apart the many
# ADM2 regions that share a name inside one country).
.global_adm_parent_cache <- new.env(parent = emptyenv())

#' Folder holding the global administrative-boundary shapefiles (internal)
#' @noRd
global_adm_dir <- function(base_dir) {
  file.path(base_dir, "spatial_separation", "global_adm_regions")
}

#' Path to the shapefile for a level key ("adm0"/"adm1"/"adm2") (internal)
#' @noRd
global_adm_shp_path <- function(adm_dir, level) {
  n <- unname(GLOBAL_ADM_LEVELS[level])
  if (length(n) != 1 || is.na(n)) {
    stop("Unknown global administrative level: ", level)
  }
  file.path(adm_dir, sprintf("geoBoundariesCGAZ_ADM%d", n), sprintf("geoBoundariesCGAZ_ADM%d.shp", n))
}

#' Label for a region without a name in the source data (internal)
#' @noRd
GLOBAL_ADM_UNNAMED_LABEL <- "(unnamed)"

#' Read only the attribute table of one administrative level (fast, no geometry) (internal)
#' @return tibble(region_code, name, country_code); `name` is the raw shapeName
#'   (empty names become "(unnamed)")
#' @noRd
read_global_adm_attributes <- function(adm_dir, level) {
  dbf <- sub("\\.shp$", ".dbf", global_adm_shp_path(adm_dir, level))
  if (!file.exists(dbf)) {
    stop("Global administrative attribute table not found: ", dbf)
  }
  d <- sf::st_read(dbf, quiet = TRUE)
  required <- if (identical(level, "adm0")) c("shapeGroup", "shapeName") else c("shapeGroup", "shapeName", "shapeID")
  missing <- setdiff(required, names(d))
  if (length(missing) > 0) {
    stop("Global administrative table is missing column(s): ", paste(missing, collapse = ", "), " (", dbf, ")")
  }
  name <- trimws(enc2utf8(as.character(d$shapeName)))
  name[is.na(name) | !nzchar(name)] <- GLOBAL_ADM_UNNAMED_LABEL
  code <- if (identical(level, "adm0")) d$shapeGroup else d$shapeID
  tibble::tibble(
    region_code = trimws(as.character(code)),
    name = name,
    country_code = trimws(as.character(d$shapeGroup))
  ) |>
    dplyr::filter(is_non_empty_chr(.data$region_code), is_non_empty_chr(.data$country_code))
}

#' Spatial-separation data for the global administrative regions: attributes now,
#' polygons lazily (internal)
#' @return list(global_adm = list(dir, attributes = list(adm0, adm1, adm2))), or NULL when the
#'   shapefiles are not available
#' @noRd
load_global_adm_spatial_data <- function(base_dir) {
  if (is.null(base_dir) || !nzchar(base_dir)) {
    return(NULL)
  }
  dir <- global_adm_dir(base_dir)
  if (!dir.exists(dir)) {
    return(NULL)
  }
  attrs <- tryCatch(
    stats::setNames(
      lapply(names(GLOBAL_ADM_LEVELS), function(lv) read_global_adm_attributes(dir, lv)),
      names(GLOBAL_ADM_LEVELS)
    ),
    error = function(e) {
      message("[global_adm] Could not read administrative attributes: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(attrs)) {
    return(NULL)
  }
  list(global_adm = list(dir = dir, attributes = attrs), warnings = character())
}

#' Spatial-separation data for Global mode: basins and administrative regions (internal)
#' @return list(global_basins =, global_adm =, warnings), or NULL when neither is available
#' @noRd
load_global_spatial_data <- function(base_dir) {
  basins <- load_global_basin_spatial_data(base_dir)
  adm <- load_global_adm_spatial_data(base_dir)
  if (is.null(basins) && is.null(adm)) {
    return(NULL)
  }
  c(
    if (!is.null(basins)) basins[setdiff(names(basins), "warnings")],
    if (!is.null(adm)) adm[setdiff(names(adm), "warnings")],
    list(warnings = unique(c(basins$warnings, adm$warnings)))
  )
}

#' Country choices (ISO3 code, name) for the administrative selector, sorted by name (internal)
#' @noRd
get_global_adm_country_choices <- function(spatial_data) {
  a0 <- spatial_data$global_adm$attributes$adm0
  if (is.null(a0) || nrow(a0) == 0) {
    return(tibble::tibble(region_code = character(), region_label = character()))
  }
  a0 |>
    dplyr::transmute(region_code = .data$region_code, region_label = .data$name) |>
    dplyr::arrange(.data$region_label)
}

#' ADM2 -> parent ADM1 name for one country (internal)
#'
#' @description Many ADM2 regions share a name inside a country (601 of Brazil's 5,570
#'   municipalities do), so the parent ADM1 name is added to the label. The parent is the ADM1
#'   polygon containing the ADM2 polygon's interior point; NA when none does.
#' @return tibble(region_code, parent)
#' @noRd
get_global_adm2_parent_names <- function(spatial_data, country_code) {
  key <- paste(spatial_data$global_adm$dir, country_code, sep = "|")
  if (!is.null(.global_adm_parent_cache[[key]])) {
    return(.global_adm_parent_cache[[key]])
  }
  out <- tibble::tibble(region_code = character(), parent = character())
  res <- tryCatch(
    {
      adm2 <- read_global_adm_polygons(spatial_data, "adm2", country_code, whole_countries = TRUE)
      adm1 <- read_global_adm_polygons(spatial_data, "adm1", country_code, whole_countries = TRUE)
      if (!is.null(adm2) && !is.null(adm1) && nrow(adm2) > 0 && nrow(adm1) > 0) {
        old_s2 <- suppressMessages(sf::sf_use_s2(FALSE))
        on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
        pts <- suppressWarnings(sf::st_point_on_surface(adm2["region_code"]))
        joined <- suppressMessages(sf::st_join(pts, adm1["name"], join = sf::st_within, left = TRUE)) |>
          sf::st_drop_geometry() |>
          dplyr::distinct(.data$region_code, .keep_all = TRUE)
        tibble::tibble(region_code = joined$region_code, parent = as.character(joined$name))
      } else {
        out
      }
    },
    error = function(e) {
      message("[global_adm] Could not determine parent ADM1 names for ", country_code, ": ", conditionMessage(e))
      out
    }
  )
  assign(key, res, envir = .global_adm_parent_cache)
  res
}

#' Make region labels unique within one choice list (internal)
#' @description Labels that are still identical after adding the country (and, for ADM2, the
#'   parent ADM1) get a stable " [n]" suffix ordered by region code.
#' @noRd
make_global_adm_labels_unique <- function(df) {
  if (nrow(df) == 0) {
    return(df)
  }
  df <- df[order(df$region_code), , drop = FALSE]
  dup <- duplicated(df$region_label) | duplicated(df$region_label, fromLast = TRUE)
  n <- stats::ave(seq_len(nrow(df)), df$region_label, FUN = seq_along)
  df$region_label[dup] <- paste0(df$region_label[dup], " [", n[dup], "]")
  df
}

#' Region choices (code, label, country) of a level inside the given countries (internal)
#'
#' @param level "adm0", "adm1" or "adm2"
#' @param countries ISO3 codes; NULL/empty means every country (ADM0 and ADM1 only -- the ADM2
#'   list is only built for explicit countries, it has 49k entries)
#' @return tibble(region_code, region_label, country_code), sorted by label. Labels are
#'   "Name (ISO3)" for ADM1 and "Name in Parent ADM1 (ISO3)" for ADM2 (no commas: stored labels
#'   are previewed comma-joined).
#' @noRd
get_global_adm_region_choices <- function(spatial_data, level, countries = NULL) {
  empty <- tibble::tibble(region_code = character(), region_label = character(), country_code = character())
  attrs <- spatial_data$global_adm$attributes[[level]]
  if (is.null(attrs) || nrow(attrs) == 0) {
    return(empty)
  }
  countries <- unique(as.character(countries))
  countries <- countries[is_non_empty_chr(countries)]
  if (length(countries) > 0) {
    attrs <- attrs[attrs$country_code %in% countries, , drop = FALSE]
  } else if (identical(level, "adm2")) {
    return(empty)
  }
  if (nrow(attrs) == 0) {
    return(empty)
  }

  if (identical(level, "adm0")) {
    return(attrs |>
      dplyr::transmute(region_code = .data$region_code, region_label = .data$name, country_code = .data$country_code) |>
      dplyr::arrange(.data$region_label))
  }

  out <- attrs |>
    dplyr::transmute(
      region_code = .data$region_code,
      name = .data$name,
      country_code = .data$country_code
    )
  out$parent <- NA_character_
  if (identical(level, "adm2")) {
    parents <- dplyr::bind_rows(lapply(unique(out$country_code), function(cc) get_global_adm2_parent_names(spatial_data, cc)))
    out$parent <- parents$parent[match(out$region_code, parents$region_code)]
  }
  out$region_label <- ifelse(
    !is.na(out$parent) & nzchar(out$parent),
    paste0(out$name, " in ", out$parent, " (", out$country_code, ")"),
    paste0(out$name, " (", out$country_code, ")")
  )
  out |>
    dplyr::select("region_code", "region_label", "country_code") |>
    make_global_adm_labels_unique() |>
    dplyr::arrange(.data$region_label)
}

#' Turn the selector inputs into the event's level / codes / labels (internal)
#'
#' @description
#' * ADM0: the chosen countries are the selection.
#' * ADM1 / ADM2 with regions chosen: exactly those regions.
#' * ADM1 / ADM2 with countries but no regions: every region of those countries, stored
#'   explicitly as the countries themselves (level "adm0"), so the saved event says what it covers.
#' * Nothing chosen: the whole area (level "global").
#' @return list(level, codes, labels)
#' @noRd
build_global_adm_selection <- function(level, countries, regions, spatial_data) {
  clean <- function(x) {
    x <- unique(as.character(x))
    x[is_non_empty_chr(x)]
  }
  level <- tolower(trimws(as.character(level)))
  countries <- clean(countries)
  regions <- clean(regions)
  whole <- list(level = GLOBAL_WHOLE_AREA_LEVEL, codes = character(0), labels = character(0))
  if (is.na(level) || !level %in% names(GLOBAL_ADM_LEVELS)) {
    return(whole)
  }

  country_selection <- function(codes) {
    ch <- get_global_adm_country_choices(spatial_data)
    list(level = "adm0", codes = codes, labels = ch$region_label[match(codes, ch$region_code)])
  }

  if (identical(level, "adm0")) {
    return(if (length(countries) == 0) whole else country_selection(countries))
  }
  if (length(regions) == 0) {
    return(if (length(countries) == 0) whole else country_selection(countries))
  }
  choices <- get_global_adm_region_choices(spatial_data, level, unique(c(
    countries,
    spatial_data$global_adm$attributes[[level]]$country_code[
      match(regions, spatial_data$global_adm$attributes[[level]]$region_code)
    ]
  )))
  list(level = level, codes = regions, labels = choices$region_label[match(regions, choices$region_code)])
}

#' Repaired polygons for a level and set of countries (internal)
#'
#' @description Reads only the requested countries from the shapefile (OGR attribute filter),
#'   repairs invalid geometries and caches the result per (level, country) for the session.
#' @param codes region codes (ISO3 for ADM0, shapeID for ADM1/ADM2) -- or, with
#'   `whole_countries = TRUE`, ISO3 country codes whose every region is wanted
#' @return sf (WGS84, MULTIPOLYGON) with region_code, name, country_code; or NULL when nothing matches
#' @noRd
read_global_adm_polygons <- function(spatial_data, level, codes, whole_countries = FALSE) {
  gadm <- spatial_data$global_adm
  attrs <- gadm$attributes[[level]]
  if (is.null(gadm) || is.null(attrs)) {
    return(NULL)
  }
  codes <- unique(as.character(codes))
  codes <- codes[is_non_empty_chr(codes)]
  if (length(codes) == 0) {
    return(NULL)
  }
  countries <- if (whole_countries) codes else unique(attrs$country_code[attrs$region_code %in% codes])
  countries <- countries[grepl("^[A-Za-z0-9]{3}$", countries)]
  if (length(countries) == 0) {
    return(NULL)
  }

  shp <- global_adm_shp_path(gadm$dir, level)
  if (!file.exists(shp)) {
    stop("Global administrative shapefile not found: ", shp)
  }
  info <- file.info(shp)
  layer_name <- tools::file_path_sans_ext(basename(shp))

  parts <- lapply(countries, function(cc) {
    key <- paste(shp, info$mtime, info$size, cc, sep = "|")
    hit <- .global_adm_cache[[key]]
    if (!is.null(hit)) {
      return(hit)
    }
    layer <- sf::st_read(
      shp,
      query = sprintf("SELECT * FROM %s WHERE shapeGroup = '%s'", layer_name, cc),
      quiet = TRUE
    )
    if (nrow(layer) == 0) {
      return(NULL)
    }
    if (!is.na(sf::st_crs(layer)) && !identical(sf::st_crs(layer)$epsg, 4326L)) {
      layer <- sf::st_transform(layer, 4326)
    }
    old_s2 <- suppressMessages(sf::sf_use_s2(FALSE))
    on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
    valid <- suppressWarnings(sf::st_is_valid(layer))
    invalid <- which(is.na(valid) | !valid)
    if (length(invalid) > 0) {
      layer[invalid, ] <- sf::st_make_valid(layer[invalid, , drop = FALSE])
    }
    name <- trimws(enc2utf8(as.character(layer$shapeName)))
    name[is.na(name) | !nzchar(name)] <- GLOBAL_ADM_UNNAMED_LABEL
    out <- tibble::tibble(
      region_code = trimws(as.character(if (identical(level, "adm0")) layer$shapeGroup else layer$shapeID)),
      name = name,
      country_code = trimws(as.character(layer$shapeGroup))
    )
    out <- sf::st_sf(out, geometry = sf::st_geometry(layer))
    out <- as_multipolygon_layer(out)
    assign(key, out, envir = .global_adm_cache)
    out
  })
  layer <- do.call(rbind, parts[!vapply(parts, is.null, logical(1))])
  if (is.null(layer) || nrow(layer) == 0) {
    return(NULL)
  }
  if (!whole_countries) {
    layer <- layer[layer$region_code %in% codes, , drop = FALSE]
  }
  if (nrow(layer) == 0) NULL else layer
}

#' Evaluate which assets fall inside the selected administrative regions (internal)
#'
#' @description Point-in-polygon on asset coordinates, exactly like the global basin selection:
#'   assets without coordinates cannot be placed and are flagged "insufficient location data".
#' @param res tibble(asset, spatial_included, spatial_multiplier, spatial_exposure_status),
#'   pre-initialised as "not exposed" by the caller
#' @return `res`, updated
#' @noRd
evaluate_global_adm_selection <- function(asset_rows, level, selected_codes, spatial_data, res) {
  if (is.null(spatial_data$global_adm)) {
    res$spatial_exposure_status <- spatial_status_insufficient()
    return(res)
  }
  selected_layer <- read_global_adm_polygons(spatial_data, level, selected_codes)

  has_coords <- !is.na(asset_rows$latitude) & !is.na(asset_rows$longitude)
  if (!is.null(selected_layer) && nrow(selected_layer) > 0 && any(has_coords)) {
    pts <- asset_rows[has_coords, , drop = FALSE]
    pts_sf <- sf::st_as_sf(pts, coords = c("longitude", "latitude"), crs = 4326)
    hits <- safe_spatial_region_join(
      pts_sf = pts_sf,
      selected_layer = selected_layer,
      layer_name = paste(level, "global administrative layer")
    )
    matched <- as.character(hits$asset[!is.na(hits$region_code)])
    idx <- res$asset %in% matched
    res$spatial_included[idx] <- TRUE
    res$spatial_multiplier[idx] <- 1
    res$spatial_exposure_status[idx] <- NA_character_
  }

  if (any(!has_coords)) {
    no_coord <- as.character(asset_rows$asset[!has_coords])
    res$spatial_exposure_status[res$asset %in% no_coord] <- spatial_status_insufficient()
  }
  res
}

#' Selected administrative-region polygons for the status map, simplified for display (internal)
#'
#' @description One feature per region (hover shows its name). A selection of more than
#'   `max_regions` regions (e.g. a whole country's municipalities) is dissolved into one feature
#'   per country instead, because the map draws one trace per feature.
#' @param event_row one-row events table with spatial_level / spatial_region_codes
#' @return sf with event_id, region_code, region_label, or NULL when nothing is selected
#' @noRd
resolve_global_adm_event_geometry <- function(event_row, spatial_data, tolerance = NULL, max_regions = 100L) {
  level <- tolower(trimws(as.character(event_row$spatial_level[[1]])))
  if (is.na(level) || !level %in% names(GLOBAL_ADM_LEVELS)) {
    return(NULL)
  }
  codes <- parse_status_spatial_values(event_row$spatial_region_codes)
  if (length(codes) == 0 || is.null(spatial_data$global_adm)) {
    return(NULL)
  }
  sel <- read_global_adm_polygons(spatial_data, level, codes)
  if (is.null(sel) || nrow(sel) == 0) {
    return(NULL)
  }
  if (is.null(tolerance)) {
    tolerance <- switch(level, adm0 = 0.05, adm1 = 0.02, 0.01)
  }

  old_s2 <- suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)

  if (nrow(sel) > max_regions) {
    countries <- get_global_adm_country_choices(spatial_data)
    pieces <- lapply(split(seq_len(nrow(sel)), sel$country_code), function(i) {
      cc <- sel$country_code[i[[1]]]
      nm <- countries$region_label[match(cc, countries$region_code)]
      sf::st_sf(
        region_code = cc,
        region_label = paste0(nm, ": ", length(i), " regions"),
        geometry = suppressMessages(suppressWarnings(sf::st_union(sf::st_geometry(sel)[i])))
      )
    })
    sel <- do.call(rbind, pieces)
  } else {
    sel <- sel |> dplyr::transmute(region_code = .data$region_code, region_label = .data$name)
  }

  geom <- suppressWarnings(sf::st_simplify(sf::st_geometry(sel), dTolerance = tolerance, preserveTopology = TRUE))
  sel <- as_multipolygon_layer(sf::st_set_geometry(sel, geom))
  sel$event_id <- as.character(event_row$event_id[[1]])
  sel[, c("event_id", "region_code", "region_label")]
}

#' Selected-area polygons for the Global status map, for either basin or administrative events (internal)
#' @noRd
resolve_global_event_geometry <- function(event_row, spatial_data, ...) {
  level <- tolower(trimws(as.character(event_row$spatial_level[[1]])))
  if (is.na(level)) {
    return(NULL)
  }
  if (level %in% names(GLOBAL_BASIN_LEVELS)) {
    return(resolve_global_basin_event_geometry(event_row, spatial_data, ...))
  }
  if (level %in% names(GLOBAL_ADM_LEVELS)) {
    return(resolve_global_adm_event_geometry(event_row, spatial_data, ...))
  }
  NULL
}
