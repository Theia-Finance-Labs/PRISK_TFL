# Global basin spatial separation (Global mode, Flood, deterministic).
#
# Shapefiles (HydroBASINS-derived, named): `named_level2_global.shp` (62 basins) and
# `named_level4_global.shp` (1,342 basins) under
# `{base_dir}/spatial_separation/global_basins/hybas_global_named/`. Attributes used:
# HYBAS_ID (code), BASIN_NAME (label), MACRO_REG (macro region) plus the polygon.
#
# Loading is two-stage so app start-up stays instant:
#   1. attributes only (the .dbf, ~0.3 s) -- enough to populate the UI dropdowns;
#   2. polygons (repaired -- most are invalid as shipped -- and cached) on first
#      use by the spatial join or the status map.

#' Levels offered for global basin separation (internal)
#' @noRd
GLOBAL_BASIN_LEVELS <- c(level2 = 2L, level4 = 4L)

#' Sentinel `spatial_level` meaning "whole selected area, no basin filter" (internal)
#' @noRd
GLOBAL_WHOLE_AREA_LEVEL <- "global"

#' Levels that mean "no sub-region filter" (Brazil's sentinel plus Global's) (internal)
#' @noRd
whole_area_spatial_levels <- function() {
  c("brazil", GLOBAL_WHOLE_AREA_LEVEL)
}

# Session cache of repaired basin polygons, keyed by file path + mtime + size.
.global_basins_cache <- new.env(parent = emptyenv())

#' Folder holding the named global basin shapefiles (internal)
#' @noRd
global_basins_dir <- function(base_dir) {
  file.path(base_dir, "spatial_separation", "global_basins", "hybas_global_named")
}

#' Path to the basin shapefile for a level key ("level2"/"level4") (internal)
#' @noRd
global_basin_shp_path <- function(basins_dir, level) {
  n <- unname(GLOBAL_BASIN_LEVELS[level])
  if (length(n) != 1 || is.na(n)) {
    stop("Unknown global basin level: ", level)
  }
  file.path(basins_dir, sprintf("named_level%d_global.shp", n))
}

#' Read only the basin attribute table (fast, no geometry) (internal)
#' @return tibble(region_code, region_label, macro_region, description)
#' @noRd
read_global_basin_attributes <- function(basins_dir, level) {
  dbf <- sub("\\.shp$", ".dbf", global_basin_shp_path(basins_dir, level))
  if (!file.exists(dbf)) {
    stop("Global basin attribute table not found: ", dbf)
  }
  d <- sf::st_read(dbf, quiet = TRUE)
  names(d) <- toupper(names(d))
  required <- c("HYBAS_ID", "BASIN_NAME", "MACRO_REG")
  missing <- setdiff(required, names(d))
  if (length(missing) > 0) {
    stop("Global basin table is missing column(s): ", paste(missing, collapse = ", "), " (", dbf, ")")
  }
  tibble::tibble(
    region_code = format(d$HYBAS_ID, scientific = FALSE, trim = TRUE),
    region_label = trimws(as.character(d$BASIN_NAME)),
    macro_region = trimws(as.character(d$MACRO_REG)),
    description = if ("DESC" %in% names(d)) as.character(d$DESC) else NA_character_
  ) |>
    dplyr::filter(is_non_empty_chr(.data$region_code), is_non_empty_chr(.data$region_label))
}

#' Spatial-separation data for Global mode: attributes now, polygons lazily (internal)
#'
#' @param base_dir data root
#' @return list(global_basins = list(dir, attributes = list(level2, level4)), warnings), or NULL
#'   when the shapefiles are not available
#' @noRd
load_global_basin_spatial_data <- function(base_dir) {
  if (is.null(base_dir) || !nzchar(base_dir)) {
    return(NULL)
  }
  dir <- global_basins_dir(base_dir)
  if (!dir.exists(dir)) {
    return(NULL)
  }
  attrs <- tryCatch(
    stats::setNames(
      lapply(names(GLOBAL_BASIN_LEVELS), function(lv) read_global_basin_attributes(dir, lv)),
      names(GLOBAL_BASIN_LEVELS)
    ),
    error = function(e) {
      message("[global_basins] Could not read basin attributes: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(attrs)) {
    return(NULL)
  }
  list(global_basins = list(dir = dir, attributes = attrs), warnings = character())
}

#' Macro regions available at a basin level, sorted (internal)
#' @noRd
get_global_basin_macro_regions <- function(spatial_data, level) {
  attrs <- spatial_data$global_basins$attributes[[level]]
  if (is.null(attrs) || nrow(attrs) == 0) {
    return(character(0))
  }
  sort(unique(attrs$macro_region[is_non_empty_chr(attrs$macro_region)]))
}

#' Basin choices (code, label, macro region) for a level, optionally within a macro region (internal)
#' @param macro_region NULL / "" / "__all__" / "Global" for all basins
#' @noRd
get_global_basin_choices <- function(spatial_data, level, macro_region = NULL) {
  attrs <- spatial_data$global_basins$attributes[[level]]
  if (is.null(attrs) || nrow(attrs) == 0) {
    return(tibble::tibble(region_code = character(), region_label = character(), macro_region = character()))
  }
  out <- attrs |>
    dplyr::select("region_code", "region_label", "macro_region")
  if (!is.null(macro_region) && length(macro_region) == 1 && is_non_empty_chr(macro_region) &&
    !macro_region %in% c("__all__", "Global")) {
    out <- out |> dplyr::filter(.data$macro_region == !!macro_region)
  }
  out |> dplyr::arrange(.data$region_label)
}

#' Force a polygon layer to a single MULTIPOLYGON geometry type (internal)
#'
#' @description Repaired basins come back as a mix of POLYGON and MULTIPOLYGON (sometimes
#'   GEOMETRYCOLLECTION), i.e. an `sfc_GEOMETRY` column, which plotly's `add_sf()`
#'   cannot draw. One row per basin is preserved.
#' @param x sf object
#' @noRd
as_multipolygon_layer <- function(x) {
  geom <- sf::st_geometry(x)
  types <- as.character(sf::st_geometry_type(geom))
  if (any(types == "GEOMETRYCOLLECTION")) {
    geom <- sf::st_sfc(
      lapply(seq_along(geom), function(i) {
        g <- geom[i]
        if (types[[i]] != "GEOMETRYCOLLECTION") {
          return(sf::st_cast(g, "MULTIPOLYGON")[[1]])
        }
        poly <- suppressWarnings(sf::st_collection_extract(g, "POLYGON"))
        if (length(poly) == 0) {
          return(sf::st_multipolygon())
        }
        # extraction yields one polygon per part: merge them back into one multipolygon
        sf::st_cast(sf::st_combine(poly), "MULTIPOLYGON")[[1]]
      }),
      crs = sf::st_crs(x)
    )
  } else {
    geom <- sf::st_cast(geom, "MULTIPOLYGON")
  }
  sf::st_set_geometry(x, geom)
}

#' Load (and cache) the repaired basin polygons for a level (internal)
#'
#' @description Most polygons in the shipped shapefiles are invalid (self-intersections),
#'   which makes spatial joins error, so they are repaired once with `st_make_valid()`
#'   and cached for the session. Columns: region_code, region_label, macro_region, geometry.
#' @return sf, WGS84
#' @noRd
load_global_basin_layer <- function(basins_dir, level) {
  shp <- global_basin_shp_path(basins_dir, level)
  if (!file.exists(shp)) {
    stop("Global basin shapefile not found: ", shp)
  }
  info <- file.info(shp)
  key <- paste(shp, info$mtime, info$size, sep = "|")
  if (!is.null(.global_basins_cache[[key]])) {
    return(.global_basins_cache[[key]])
  }

  # On-disk cache of the repaired polygons (the repair takes ~10 s per level), keyed by
  # the shapefile's mtime + size so an updated shapefile is re-processed automatically.
  disk_cache <- tryCatch(
    file.path(
      tools::R_user_dir("climate.risk.tool", "cache"),
      sprintf("global_basins_v2_%s_%s_%s.rds", level, round(as.numeric(info$mtime)), info$size)
    ),
    error = function(e) NA_character_
  )
  if (!is.na(disk_cache) && file.exists(disk_cache)) {
    cached <- tryCatch(readRDS(disk_cache), error = function(e) NULL)
    if (inherits(cached, "sf")) {
      assign(key, cached, envir = .global_basins_cache)
      return(cached)
    }
  }

  message("[global_basins] Loading and repairing ", basename(shp), " ...")
  layer <- sf::st_read(shp, quiet = TRUE)
  names(layer) <- ifelse(names(layer) == "geometry", names(layer), toupper(names(layer)))
  if (!is.na(sf::st_crs(layer)) && !identical(sf::st_crs(layer)$epsg, 4326L)) {
    layer <- sf::st_transform(layer, 4326)
  }

  old_s2 <- suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
  invalid <- which(is.na(suppressWarnings(sf::st_is_valid(layer))) | !suppressWarnings(sf::st_is_valid(layer)))
  if (length(invalid) > 0) {
    layer[invalid, ] <- sf::st_make_valid(layer[invalid, , drop = FALSE])
  }

  layer <- layer |>
    dplyr::transmute(
      region_code = format(.data$HYBAS_ID, scientific = FALSE, trim = TRUE),
      region_label = trimws(as.character(.data$BASIN_NAME)),
      macro_region = trimws(as.character(.data$MACRO_REG))
    )
  layer <- as_multipolygon_layer(layer)
  assign(key, layer, envir = .global_basins_cache)
  if (!is.na(disk_cache)) {
    try({
      dir.create(dirname(disk_cache), recursive = TRUE, showWarnings = FALSE)
      saveRDS(layer, disk_cache)
    }, silent = TRUE)
  }
  layer
}

#' Basin polygons for a level from the spatial-separation data (internal)
#' @noRd
get_global_basin_layer <- function(spatial_data, level) {
  gb <- spatial_data$global_basins
  if (is.null(gb) || is.null(gb$dir)) {
    return(NULL)
  }
  load_global_basin_layer(gb$dir, level)
}

#' Evaluate which assets fall inside the selected basins (internal)
#'
#' @description Point-in-polygon on asset coordinates. Assets without coordinates
#'   cannot be placed in a basin and are flagged "insufficient location data" (and
#'   excluded), mirroring Brazil's behaviour when a selection cannot be resolved.
#' @param res tibble(asset, spatial_included, spatial_multiplier, spatial_exposure_status),
#'   pre-initialised as "not exposed" by the caller
#' @return `res`, updated
#' @noRd
evaluate_global_basin_selection <- function(asset_rows, level, selected_codes, spatial_data, res) {
  layer <- get_global_basin_layer(spatial_data, level)
  if (is.null(layer) || nrow(layer) == 0) {
    res$spatial_exposure_status <- spatial_status_insufficient()
    return(res)
  }

  selected_layer <- layer |>
    dplyr::filter(.data$region_code %in% selected_codes)
  has_coords <- !is.na(asset_rows$latitude) & !is.na(asset_rows$longitude)

  if (nrow(selected_layer) > 0 && any(has_coords)) {
    pts <- asset_rows[has_coords, , drop = FALSE]
    pts_sf <- sf::st_as_sf(pts, coords = c("longitude", "latitude"), crs = 4326)
    hits <- safe_spatial_region_join(
      pts_sf = pts_sf,
      selected_layer = selected_layer,
      layer_name = paste(level, "global basin layer")
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

#' Selected-basin polygons for the status map, simplified for display (internal)
#'
#' @param event_row one-row events table with spatial_level / spatial_region_codes
#' @param tolerance simplification tolerance in degrees
#' @return sf with an `event_id` column, or NULL when nothing is selected
#' @noRd
resolve_global_basin_event_geometry <- function(event_row, spatial_data, tolerance = NULL) {
  level <- tolower(trimws(as.character(event_row$spatial_level[[1]])))
  if (is.na(level) || !level %in% names(GLOBAL_BASIN_LEVELS)) {
    return(NULL)
  }
  codes <- parse_status_spatial_values(event_row$spatial_region_codes)
  if (length(codes) == 0) {
    return(NULL)
  }
  layer <- get_global_basin_layer(spatial_data, level)
  if (is.null(layer)) {
    return(NULL)
  }
  sel <- layer[layer$region_code %in% codes, , drop = FALSE]
  if (nrow(sel) == 0) {
    return(NULL)
  }
  if (is.null(tolerance)) {
    tolerance <- if (identical(level, "level2")) 0.15 else 0.05
  }
  old_s2 <- suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
  geom <- suppressWarnings(sf::st_simplify(sf::st_geometry(sel), dTolerance = tolerance, preserveTopology = TRUE))
  sel <- as_multipolygon_layer(sf::st_set_geometry(sel, geom))
  sel$event_id <- as.character(event_row$event_id[[1]])
  sel[, c("event_id", "region_code", "region_label", "macro_region")]
}
