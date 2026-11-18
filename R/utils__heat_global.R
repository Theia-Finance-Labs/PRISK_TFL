# Global heat hazard (labour productivity) -- Global mode only.
#
# Brazil's Heat hazard (NetCDF, GWL-indexed, CNAE/state factors) is completely
# untouched: everything here is gated on a hazard config whose indicator is
# declared `type: global_heat_maps` (see `{base_dir}/hazards/config_global/Heat.yml`).
#
# Data: 20-year "change" GeoTIFFs (one per SSP x window), 29 bands each. Only the
# `excess_<statistic>_RP<n>` bands are used (days/year of heat-index >= 35C above
# the historical baseline). Each map represents the climate of its centre year
# (e.g. 2021-2040 -> 2030.5); a shock year between two centres is linearly blended.

#' Labour-productivity loss by exposure class (hardcoded by design)
#' @noRd
HEAT_LP_LOSS_BY_EXPOSURE <- c(high = 0.208, medium = 0.118, low = 0.042)

#' Exposure class assigned when an asset's sector cannot be matched to a NACE code
#' @noRd
HEAT_DEFAULT_EXPOSURE <- "Medium"

#' Heat-map statistics available as bands (median is the default)
#' @noRd
HEAT_STATISTICS <- c("median", "mean", "p10", "p90")

# ---------------------------------------------------------------------------
# Config helpers
# ---------------------------------------------------------------------------

#' TRUE when a hazard config describes the global heat-map engine (internal)
#' @param cfg A normalized hazard config (one element of `hazard_configs`)
#' @noRd
is_global_heat_config <- function(cfg) {
  if (is.null(cfg) || is.null(cfg$indicators) || length(cfg$indicators) == 0) {
    return(FALSE)
  }
  any(vapply(
    cfg$indicators,
    function(ind) identical(as.character(ind$type), "global_heat_maps"),
    logical(1)
  ))
}

#' Names of hazard types that use the global heat-map engine (internal)
#' @param hazard_configs Named list of hazard configs
#' @noRd
global_heat_hazard_types <- function(hazard_configs) {
  if (is.null(hazard_configs) || length(hazard_configs) == 0) {
    return(character(0))
  }
  names(hazard_configs)[vapply(hazard_configs, is_global_heat_config, logical(1))]
}

#' Logical vector: which event rows are global-heat events (internal)
#' @noRd
is_global_heat_event <- function(events, hazard_configs) {
  if (is.null(events) || nrow(events) == 0 || !"hazard_type" %in% names(events)) {
    return(logical(0))
  }
  as.character(events$hazard_type) %in% global_heat_hazard_types(hazard_configs)
}

#' Order heat statistics for display (median first = default) (internal)
#' @param x character vector of statistics
#' @noRd
order_heat_statistics <- function(x) {
  x <- as.character(x)
  c(intersect(HEAT_STATISTICS, x), setdiff(x, HEAT_STATISTICS))
}

#' Resolve the heat-maps folder from a hazards config directory (internal)
#' @param hazards_dir Path to `.../hazards/config_global`
#' @param hazard_config The global heat hazard config
#' @noRd
global_heat_maps_dir <- function(hazards_dir, hazard_config) {
  indicator <- Filter(function(ind) identical(as.character(ind$type), "global_heat_maps"), hazard_config$indicators)[[1]]
  file.path(dirname(hazards_dir), "indicators", gsub("/+$", "", indicator$file))
}

#' Drop the heat-only `statistic` inventory column (internal)
#'
#' @description `statistic` exists in the inventory only so the event UI can offer
#'   it as an index dropdown; compute-time heat logic reads it from the event row.
#'   Left in place it would leak (as an all-NA column) into every other hazard's
#'   extraction output, so compute entry points drop it up front.
#' @noRd
drop_heat_ui_inventory_columns <- function(hazards_inventory) {
  if (is.null(hazards_inventory) || !is.data.frame(hazards_inventory)) {
    return(hazards_inventory)
  }
  hazards_inventory |> dplyr::select(-dplyr::any_of("statistic"))
}

# ---------------------------------------------------------------------------
# Map discovery, inventory
# ---------------------------------------------------------------------------

#' Discover global heat change maps and their bands (internal)
#'
#' @param maps_dir Folder holding `hi35_change_<ssp>_<start>-<end>.tif`
#' @return list(files = tibble(ssp, start_year, end_year, centre_year, path),
#'   bands = tibble(statistic, return_period, band))
#' @noRd
scan_global_heat_maps <- function(maps_dir) {
  empty <- list(
    files = tibble::tibble(
      ssp = character(), start_year = integer(), end_year = integer(),
      centre_year = numeric(), path = character()
    ),
    bands = tibble::tibble(statistic = character(), return_period = numeric(), band = character())
  )
  if (is.null(maps_dir) || !dir.exists(maps_dir)) {
    return(empty)
  }

  paths <- list.files(maps_dir, pattern = "^hi35_change_.+_[0-9]{4}-[0-9]{4}\\.tif$", full.names = TRUE)
  if (length(paths) == 0) {
    return(empty)
  }

  m <- regmatches(
    basename(paths),
    regexec("^hi35_change_(.+)_([0-9]{4})-([0-9]{4})\\.tif$", basename(paths))
  )
  files <- tibble::tibble(
    ssp = vapply(m, `[[`, character(1), 2),
    start_year = as.integer(vapply(m, `[[`, character(1), 3)),
    end_year = as.integer(vapply(m, `[[`, character(1), 4)),
    path = paths
  ) |>
    dplyr::mutate(centre_year = (.data$start_year + .data$end_year) / 2) |>
    dplyr::arrange(.data$ssp, .data$centre_year) |>
    dplyr::select("ssp", "start_year", "end_year", "centre_year", "path")

  band_names <- names(terra::rast(files$path[[1]]))
  bm <- regmatches(band_names, regexec("^excess_(median|mean|p10|p90)_RP([0-9]+)$", band_names))
  bm <- bm[lengths(bm) == 3]
  bands <- tibble::tibble(
    statistic = vapply(bm, `[[`, character(1), 2),
    return_period = as.numeric(vapply(bm, `[[`, character(1), 3)),
    band = vapply(bm, `[[`, character(1), 1)
  )

  list(files = files, bands = bands)
}

#' Build the Heat inventory rows for the UI / event reconciliation (internal)
#'
#' @description One row per (scenario, return period, statistic). The map
#'   window is deliberately NOT an inventory dimension: it is derived from the
#'   shock year at compute time (see `heat_blend_weights()`).
#' @noRd
build_global_heat_inventory <- function(hazard_type, indicator_key, indicator, hazard_indicators_dir) {
  maps_dir <- file.path(hazard_indicators_dir, gsub("/+$", "", indicator$file))
  scanned <- scan_global_heat_maps(maps_dir)
  if (nrow(scanned$files) == 0 || nrow(scanned$bands) == 0) {
    message("  No global heat maps found at: ", maps_dir)
    return(NULL)
  }

  grid <- tidyr::crossing(
    scenario_name = unique(scanned$files$ssp),
    scanned$bands |> dplyr::select("return_period", "statistic")
  ) |>
    dplyr::arrange(.data$scenario_name, .data$statistic, .data$return_period)

  # Whole-number return periods are stored as integers, matching the flood
  # inventory's column type so binding the two never changes flood output types.
  if (all(grid$return_period == round(grid$return_period))) {
    grid$return_period <- as.integer(grid$return_period)
  }

  grid |>
    dplyr::mutate(
      hazard_type = hazard_type,
      hazard_indicator = indicator_key,
      indicator_key = paste0(
        "heat_global__", indicator$variable,
        "__scenario_name=", .data$scenario_name,
        "__return_period=", .data$return_period,
        "__statistic=", .data$statistic
      ),
      hazard_name = paste0(hazard_type, "__", .data$indicator_key),
      ensemble = "mean",
      season = NA_character_,
      source = "heat_maps",
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
# Time blending
# ---------------------------------------------------------------------------

#' Weights over map windows for a given calendar year (internal)
#'
#' @description Each window is treated as the climate of its centre year. A year
#'   at or before the first centre uses the first map unchanged; a year at or
#'   after the last centre uses the last map unchanged; otherwise the two maps
#'   whose centres bracket the year are blended linearly:
#'   `w = (year - earlier centre) / (later centre - earlier centre)`,
#'   `hazard = (1 - w) * earlier + w * later`.
#' @param year Numeric scalar calendar year
#' @param centre_years Numeric vector of window centre years
#' @return tibble(centre_year, weight) with only non-zero weights (weights sum to 1)
#' @noRd
heat_blend_weights <- function(year, centre_years) {
  centres <- sort(unique(as.numeric(centre_years)))
  year <- as.numeric(year)
  if (length(centres) == 0 || is.na(year)) {
    return(tibble::tibble(centre_year = numeric(), weight = numeric()))
  }
  if (year <= centres[[1]]) {
    return(tibble::tibble(centre_year = centres[[1]], weight = 1))
  }
  if (year >= centres[[length(centres)]]) {
    return(tibble::tibble(centre_year = centres[[length(centres)]], weight = 1))
  }
  i <- max(which(centres <= year))
  w <- (year - centres[[i]]) / (centres[[i + 1]] - centres[[i]])
  out <- tibble::tibble(centre_year = c(centres[[i]], centres[[i + 1]]), weight = c(1 - w, w))
  out[out$weight > 0, ]
}

#' Extract excess heat days per asset, window and return period (internal)
#'
#' @param assets data.frame with columns asset, longitude, latitude
#' @param maps_dir heat maps folder
#' @param ssp scenario, e.g. "ssp585"
#' @param statistic one of HEAT_STATISTICS
#' @param return_periods numeric vector of return periods (default: all available)
#' @param needed_centres optional numeric vector of window centre years to read
#'   (default: all windows of the scenario)
#' @return tibble(asset, centre_year, return_period, excess_days), values clamped
#'   to [0, 365]; NA for assets without usable coordinates
#' @noRd
extract_global_heat_excess <- function(assets, maps_dir, ssp, statistic = "median",
                                       return_periods = NULL, needed_centres = NULL) {
  scanned <- scan_global_heat_maps(maps_dir)
  files <- scanned$files[scanned$files$ssp == ssp, , drop = FALSE]
  if (nrow(files) == 0) {
    stop("extract_global_heat_excess: no heat maps found for scenario '", ssp, "' in ", maps_dir)
  }
  if (!statistic %in% HEAT_STATISTICS) {
    stop("extract_global_heat_excess: statistic must be one of: ", paste(HEAT_STATISTICS, collapse = ", "))
  }
  bands <- scanned$bands[scanned$bands$statistic == statistic, , drop = FALSE]
  if (!is.null(return_periods)) {
    bands <- bands[bands$return_period %in% as.numeric(return_periods), , drop = FALSE]
  }
  if (nrow(bands) == 0) {
    stop("extract_global_heat_excess: no bands for statistic '", statistic, "' and the requested return periods")
  }
  if (!is.null(needed_centres)) {
    files <- files[files$centre_year %in% as.numeric(needed_centres), , drop = FALSE]
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
    vals <- pmin(pmax(vals, 0), 365)
    out[[i]] <- tibble::tibble(
      asset = rep(as.character(assets$asset), times = nrow(bands)),
      centre_year = files$centre_year[[i]],
      return_period = rep(bands$return_period, each = nrow(assets)),
      excess_days = as.vector(vals)
    )
  }
  dplyr::bind_rows(out)
}

#' Blend per-window excess days to a single value per asset and return period (internal)
#' @param excess_long output of `extract_global_heat_excess()`
#' @param year calendar year
#' @return tibble(asset, return_period, excess_days)
#' @noRd
blend_global_heat_excess <- function(excess_long, year) {
  w <- heat_blend_weights(year, unique(excess_long$centre_year))
  excess_long |>
    dplyr::inner_join(w, by = "centre_year") |>
    dplyr::group_by(.data$asset, .data$return_period) |>
    dplyr::summarise(
      excess_days = if (any(is.na(.data$excess_days))) NA_real_ else sum(.data$weight * .data$excess_days),
      .groups = "drop"
    )
}

# ---------------------------------------------------------------------------
# NACE -> labour-productivity exposure
# ---------------------------------------------------------------------------

#' Normalize a NACE code to the hierarchical form used by Nace_exposure.xlsx (internal)
#'
#' @description The exposure table writes codes as `division[.group[.class]]`
#'   without leading zeros (`1`, `1.1`, `1.1.1`, `10.1.1`). Inputs may arrive as
#'   Excel floats (`1.1000000000000001`), with leading zeros, or in the standard
#'   NACE Rev.2 form (`01.11`, `0111`). All are mapped to the table's form.
#' @param x vector of codes
#' @return character vector (NA for blank / non-code input)
#' @noRd
normalize_nace_code <- function(x) {
  x <- trimws(as.character(x))
  x[!nzchar(x) | toupper(x) %in% c("NA", "N/A", "NAN")] <- NA_character_

  # Excel float artefacts, e.g. 1.1000000000000001 -> 1.1
  long_frac <- !is.na(x) & grepl("^[0-9]+[.][0-9]{7,}$", x)
  if (any(long_frac)) {
    x[long_frac] <- format(round(as.numeric(x[long_frac]), 6), trim = TRUE, drop0trailing = TRUE, scientific = FALSE)
  }

  strip_zeros <- function(d) {
    d <- sub("^0+", "", d)
    ifelse(nzchar(d), d, "0")
  }

  vapply(x, function(code) {
    if (is.na(code)) {
      return(NA_character_)
    }
    code <- gsub("\\s+", "", code)
    if (grepl("^[0-9]+$", code)) {
      # digits only: DD, DDG, DDGC (standard NACE Rev.2, division always 2 digits)
      if (nchar(code) <= 2) {
        return(strip_zeros(code))
      }
      div <- strip_zeros(substr(code, 1, 2))
      rest <- strsplit(substr(code, 3, nchar(code)), "")[[1]]
      if (length(rest) > 2) {
        return(NA_character_)
      }
      return(paste(c(div, rest), collapse = "."))
    }
    if (grepl("^[0-9]+[.][0-9]+$", code)) {
      parts <- strsplit(code, ".", fixed = TRUE)[[1]]
      div <- strip_zeros(parts[[1]])
      if (nchar(parts[[2]]) == 1) {
        return(paste(div, parts[[2]], sep = "."))
      }
      if (nchar(parts[[2]]) == 2) {
        return(paste(div, substr(parts[[2]], 1, 1), substr(parts[[2]], 2, 2), sep = "."))
      }
      return(NA_character_)
    }
    if (grepl("^[0-9]+[.][0-9][.][0-9]$", code)) {
      parts <- strsplit(code, ".", fixed = TRUE)[[1]]
      return(paste(strip_zeros(parts[[1]]), parts[[2]], parts[[3]], sep = "."))
    }
    NA_character_
  }, character(1), USE.NAMES = FALSE)
}

#' Load the NACE labour-productivity exposure table (internal)
#' @return tibble(code, description, level, exposure) with exposure in Low/Medium/High
#' @noRd
load_nace_exposure_table <- function(mappings_dir, mapping_file = "Nace_exposure.xlsx") {
  path <- file.path(mappings_dir, mapping_file)
  if (!file.exists(path)) {
    stop("NACE exposure table not found: ", path)
  }
  raw <- readxl::read_excel(path, col_types = "text")
  names(raw) <- to_snake_case(names(raw))
  required <- c("code", "description", "level", "overall_exposure")
  missing <- setdiff(required, names(raw))
  if (length(missing) > 0) {
    stop("NACE exposure table is missing column(s): ", paste(missing, collapse = ", "), " (", path, ")")
  }
  exposure <- tools::toTitleCase(tolower(trimws(raw$overall_exposure)))
  tibble::tibble(
    code = normalize_nace_code(raw$code),
    description = trimws(as.character(raw$description)),
    level = trimws(as.character(raw$level)),
    exposure = exposure
  ) |>
    dplyr::filter(.data$exposure %in% c("Low", "Medium", "High"))
}

#' Resolve each asset's labour-productivity exposure from its NACE code / sector (internal)
#'
#' @description Matching order per asset: (1) exact normalized NACE code,
#'   (2) the same code with trailing levels dropped (class -> group -> division),
#'   (3) the sector text against the table's descriptions (broadest level wins),
#'   (4) the default exposure (Medium) with a warning.
#' @param sector_values character vector (NACE code or sector description)
#' @param nace_table output of `load_nace_exposure_table()`
#' @return tibble(nace_input, nace_code, nace_level, nace_description, lp_exposure, nace_match)
#' @noRd
resolve_nace_exposure <- function(sector_values, nace_table) {
  raw <- as.character(sector_values)
  norm <- normalize_nace_code(raw)

  code_tbl <- nace_table[!is.na(nace_table$code), , drop = FALSE]
  code_tbl <- code_tbl[!duplicated(code_tbl$code), , drop = FALSE]

  level_rank <- c(Section = 1, Division = 2, Group = 3, Class = 4)
  desc_tbl <- nace_table
  desc_tbl$rank <- unname(level_rank[desc_tbl$level])
  desc_tbl$desc_key <- tolower(trimws(desc_tbl$description))
  desc_tbl <- desc_tbl[order(desc_tbl$rank, na.last = TRUE), , drop = FALSE]
  desc_tbl <- desc_tbl[!duplicated(desc_tbl$desc_key), , drop = FALSE]

  n <- length(raw)
  out <- tibble::tibble(
    nace_input = raw,
    nace_code = rep(NA_character_, n),
    nace_level = rep(NA_character_, n),
    nace_description = rep(NA_character_, n),
    lp_exposure = rep(NA_character_, n),
    nace_match = rep(NA_character_, n)
  )

  for (i in seq_len(n)) {
    code <- norm[[i]]
    if (!is.na(code)) {
      candidates <- code
      parts <- strsplit(code, ".", fixed = TRUE)[[1]]
      while (length(parts) > 1) {
        parts <- parts[-length(parts)]
        candidates <- c(candidates, paste(parts, collapse = "."))
      }
      hit <- match(candidates, code_tbl$code)
      hit_pos <- which(!is.na(hit))[1]
      if (!is.na(hit_pos)) {
        row <- code_tbl[hit[[hit_pos]], ]
        out$nace_code[[i]] <- row$code
        out$nace_level[[i]] <- row$level
        out$nace_description[[i]] <- row$description
        out$lp_exposure[[i]] <- row$exposure
        out$nace_match[[i]] <- if (hit_pos == 1) "code" else "parent_code"
        next
      }
    }
    key <- tolower(trimws(raw[[i]]))
    if (!is.na(key) && nzchar(key)) {
      j <- match(key, desc_tbl$desc_key)
      if (!is.na(j)) {
        out$nace_code[[i]] <- desc_tbl$code[[j]]
        out$nace_level[[i]] <- desc_tbl$level[[j]]
        out$nace_description[[i]] <- desc_tbl$description[[j]]
        out$lp_exposure[[i]] <- desc_tbl$exposure[[j]]
        out$nace_match[[i]] <- "description"
        next
      }
    }
    out$lp_exposure[[i]] <- HEAT_DEFAULT_EXPOSURE
    out$nace_match[[i]] <- "default"
  }
  out
}

#' Labour-productivity loss fraction for an exposure class (internal)
#' @param lp_exposure vector of "Low"/"Medium"/"High" (any case)
#' @return numeric vector
#' @noRd
heat_lp_loss_for_exposure <- function(lp_exposure) {
  unname(HEAT_LP_LOSS_BY_EXPOSURE[tolower(as.character(lp_exposure))])
}

# ---------------------------------------------------------------------------
# Labour elasticity (beta_L) by IMF country group, labour shock, output change
# ---------------------------------------------------------------------------
#
# Output change uses ONLY the labour elasticity. When only labour is shocked, the
# technology level A, capital K, energy E and their elasticities (beta_K, beta_E)
# appear identically in the baseline and the heat-adjusted output and cancel, so the
# relative output change depends only on beta_L and the labour shock s:
#
#   s                 = (additional days with HI > 35C per year / 365) x LP loss of the exposure class
#   output_change_pct = (1 - s)^beta_L - 1                       (a fraction, e.g. -0.006923 = -0.6923%)
#   revenue_adjustment = baseline_revenue x output_change_pct
#
# beta_L values (named parameters; they live in the `constants` of the revenue equation in
# `hazards/config_global/Heat.yml` -- `beta_L_advanced`, `beta_L_emde` -- so they can be swapped
# for sensitivity runs without touching code):
#   IMF "Advanced Economies"                          beta_L = 0.535
#   IMF "Emerging Market and Developing Economies"    beta_L = 0.399
#   country not in the IMF classification             EMDE (beta_L = 0.399), flagged "default_EMDE"
# Source: Songur, M. (2019), Ankara Haci Bayram Veli Universitesi Iktisadi ve Idari Bilimler
# Fakultesi Dergisi 21(3), 757-784, Table 5, p. 774 (AMG estimator; labour = employment x
# Penn World Table human capital index).

#' IMF group labels as they appear in IMF_classification.xlsx (internal)
#' @noRd
IMF_GROUP_ADVANCED <- "Advanced Economies"
IMF_GROUP_EMDE <- "Emerging Market and Developing Economies"

#' `beta_L_source` values (internal)
#' @noRd
HEAT_BETA_SOURCE_IMF <- "IMF_classification"
HEAT_BETA_SOURCE_DEFAULT <- "default_EMDE"

#' Read the beta_L parameters from the heat config (internal)
#'
#' @param hazard_config the global heat hazard config
#' @return named numeric c(advanced = , emde = )
#' @noRd
heat_beta_l_parameters <- function(hazard_config) {
  consts <- list()
  for (eq in hazard_config$shocks$revenue$equations) {
    consts <- utils::modifyList(consts, as.list(eq$constants))
  }
  needed <- c("beta_L_advanced", "beta_L_emde")
  missing <- setdiff(needed, names(consts))
  if (length(missing) > 0) {
    stop(
      "Heat config is missing the labour-elasticity parameter(s): ", paste(missing, collapse = ", "),
      " (set them as constants of the revenue equation in Heat.yml)"
    )
  }
  vals <- vapply(consts[needed], function(x) suppressWarnings(as.numeric(x)), numeric(1))
  if (anyNA(vals) || any(vals <= 0 | vals > 1)) {
    stop("beta_L_advanced and beta_L_emde must be numbers in (0, 1]; got: ", paste(vals, collapse = ", "))
  }
  c(advanced = unname(vals[["beta_L_advanced"]]), emde = unname(vals[["beta_L_emde"]]))
}

#' Labour shock s = excess days / 365 x LP loss (internal)
#' @param excess_days additional days/year with heat index > 35C
#' @param lp_loss productivity loss of the sector's exposure class (fraction)
#' @noRd
heat_labour_shock <- function(excess_days, lp_loss) {
  excess_days / 365 * lp_loss
}

#' Relative output change (1 - s)^beta_L - 1 (internal)
#' @param s labour shock (fraction)
#' @param beta_L labour elasticity
#' @return fraction (negative = output loss)
#' @noRd
heat_output_change_pct <- function(s, beta_L) {
  (1 - s)^beta_L - 1
}

#' Load the country name / ISO-code table used to identify countries (internal)
#'
#' @description Only the identifiers are used (the table's labour/capital/energy
#'   columns are no longer needed): it is the crosswalk between the IMF file (names only)
#'   and the ISO codes assets use.
#' @return tibble(iso2, iso3, name)
#' @noRd
load_country_code_table <- function(mappings_dir, mapping_file = "CB_function_data_per_country.xlsx") {
  path <- file.path(mappings_dir, mapping_file)
  if (!file.exists(path)) {
    stop("Country code table not found: ", path)
  }
  raw <- readxl::read_excel(path, col_types = "text", na = character())
  required <- c("countryRegionIso2", "countryRegionId", "countryRegionName")
  missing <- setdiff(required, names(raw))
  if (length(missing) > 0) {
    stop("Country code table is missing column(s): ", paste(missing, collapse = ", "), " (", path, ")")
  }
  tibble::tibble(
    iso2 = toupper(trimws(enc2utf8(raw$countryRegionIso2))),
    iso3 = toupper(trimws(enc2utf8(raw$countryRegionId))),
    name = trimws(enc2utf8(raw$countryRegionName))
  ) |>
    dplyr::filter(!toupper(.data$iso2) %in% c("GLOBAL-AVG"))
}

#' Case-insensitive, trimmed key for exact name matching (no accent folding, no fuzziness) (internal)
#' @noRd
country_name_key <- function(x) {
  tolower(trimws(enc2utf8(as.character(x))))
}

#' Load the IMF country classification with ISO codes attached (internal)
#'
#' @description `IMF_classification.xlsx` lists country NAMES only. ISO codes are attached by
#'   (1) exact (case-insensitive) name match against the country code table, else
#'   (2) the explicit, reviewable alias file `imf_country_aliases.csv` (columns imf_name, iso2,
#'   iso3, basis), which covers spelling differences and countries absent from the code table.
#'   Nothing is fuzzy-matched. Unknown classification labels are an error (never guessed).
#' @return tibble(imf_name, imf_group, iso2, iso3, code_source)
#' @noRd
load_imf_classification <- function(mappings_dir,
                                    mapping_file = "IMF_classification.xlsx",
                                    aliases_file = "imf_country_aliases.csv") {
  path <- file.path(mappings_dir, mapping_file)
  if (!file.exists(path)) {
    stop("IMF classification not found: ", path)
  }
  raw <- readxl::read_excel(path, col_types = "text", na = character())
  if (!all(c("Country", "Classification") %in% names(raw))) {
    stop("IMF classification must have columns 'Country' and 'Classification' (", path, ")")
  }
  imf <- tibble::tibble(
    imf_name = trimws(enc2utf8(raw$Country)),
    imf_group = trimws(enc2utf8(raw$Classification))
  ) |>
    dplyr::filter(nzchar(.data$imf_name))

  bad <- setdiff(unique(imf$imf_group), c(IMF_GROUP_ADVANCED, IMF_GROUP_EMDE))
  if (length(bad) > 0) {
    stop("Unknown IMF classification label(s): ", paste0("'", bad, "'", collapse = ", "), " (", path, ")")
  }
  if (anyDuplicated(country_name_key(imf$imf_name)) > 0) {
    stop("IMF classification lists a country more than once: ", path)
  }

  codes <- load_country_code_table(mappings_dir)
  idx <- match(country_name_key(imf$imf_name), country_name_key(codes$name))
  imf$iso2 <- codes$iso2[idx]
  imf$iso3 <- codes$iso3[idx]
  imf$code_source <- ifelse(is.na(idx), NA_character_, "country_code_table")

  alias_path <- file.path(mappings_dir, aliases_file)
  if (file.exists(alias_path)) {
    aliases <- readr::read_csv(
      alias_path, col_types = readr::cols(.default = "c"), na = character(),
      locale = readr::locale(encoding = "UTF-8"), show_col_types = FALSE
    )
    if (!all(c("imf_name", "iso2", "iso3") %in% names(aliases))) {
      stop("Alias file must have columns imf_name, iso2, iso3: ", alias_path)
    }
    a_idx <- match(country_name_key(imf$imf_name), country_name_key(aliases$imf_name))
    use_alias <- is.na(imf$iso2) & !is.na(a_idx)
    imf$iso2[use_alias] <- toupper(trimws(aliases$iso2[a_idx[use_alias]]))
    imf$iso3[use_alias] <- toupper(trimws(aliases$iso3[a_idx[use_alias]]))
    imf$code_source[use_alias] <- "alias_file"
  }
  imf
}

#' Classify each asset country into an IMF group and assign beta_L (internal)
#'
#' @description Match order for the asset's country value: ISO2, ISO3, IMF country name, then the
#'   country-code-table name (all exact, case-insensitive; "UK" is read as "GB"). A country that
#'   is missing or not in the classification gets the EMDE default and
#'   `beta_L_source = "default_EMDE"`.
#' @param country_values character vector of asset countries
#' @param imf_table output of `load_imf_classification()`
#' @param code_table output of `load_country_code_table()`
#' @param beta_L named numeric c(advanced, emde) from `heat_beta_l_parameters()`
#' @return tibble(country_input, imf_country, imf_group, imf_match, beta_L, beta_L_source)
#' @noRd
resolve_imf_beta_l <- function(country_values, imf_table, code_table, beta_L) {
  raw <- as.character(country_values)
  key <- toupper(trimws(enc2utf8(raw)))
  key[key == "UK"] <- "GB"
  name_key <- country_name_key(raw)

  n <- length(raw)
  hit <- rep(NA_integer_, n)
  how <- rep(NA_character_, n)
  for (i in seq_len(n)) {
    if (is.na(key[[i]]) || !nzchar(key[[i]])) next
    j <- match(key[[i]], imf_table$iso2)
    h <- "iso2"
    if (is.na(j)) {
      j <- match(key[[i]], imf_table$iso3)
      h <- "iso3"
    }
    if (is.na(j)) {
      j <- match(name_key[[i]], country_name_key(imf_table$imf_name))
      h <- "imf_name"
    }
    if (is.na(j)) {
      k <- match(name_key[[i]], country_name_key(code_table$name))
      if (!is.na(k)) {
        j <- match(code_table$iso2[[k]], imf_table$iso2)
        h <- "country_code_table_name"
      }
    }
    if (!is.na(j)) {
      hit[[i]] <- j
      how[[i]] <- h
    }
  }

  found <- !is.na(hit)
  group <- ifelse(found, imf_table$imf_group[hit], IMF_GROUP_EMDE)
  out <- tibble::tibble(
    country_input = raw,
    imf_country = ifelse(found, imf_table$imf_name[hit], NA_character_),
    imf_group = group,
    imf_match = ifelse(found, how, "default_EMDE"),
    beta_L = ifelse(group == IMF_GROUP_ADVANCED, unname(beta_L[["advanced"]]), unname(beta_L[["emde"]])),
    beta_L_source = ifelse(found, HEAT_BETA_SOURCE_IMF, HEAT_BETA_SOURCE_DEFAULT)
  )
  out
}

# ---------------------------------------------------------------------------
# Asset attributes + event factor rows
# ---------------------------------------------------------------------------

#' Per-asset heat attributes: NACE exposure, LP loss, IMF group and beta_L (internal)
#' @param assets data.frame from `read_assets()`
#' @param mappings_dir folder with Nace_exposure.xlsx, IMF_classification.xlsx,
#'   imf_country_aliases.csv and the country code table
#' @param hazard_config the global heat config (source of the beta_L parameters)
#' @return tibble(asset, nace_input, nace_code, nace_level, nace_description, nace_match, lp_exposure,
#'   lp_loss, imf_country, imf_group, imf_match, beta_L, beta_L_source)
#' @noRd
build_global_heat_asset_attributes <- function(assets, mappings_dir, hazard_config) {
  nace_table <- load_nace_exposure_table(mappings_dir)
  imf_table <- load_imf_classification(mappings_dir)
  code_table <- load_country_code_table(mappings_dir)
  beta_L <- heat_beta_l_parameters(hazard_config)

  sector_col <- intersect(c("nace", "nace_code", "sector"), names(assets))
  sector_values <- if (length(sector_col) > 0) assets[[sector_col[[1]]]] else rep(NA_character_, nrow(assets))
  country_values <- if ("country" %in% names(assets)) assets$country else rep(NA_character_, nrow(assets))

  nace <- resolve_nace_exposure(sector_values, nace_table)
  imf <- resolve_imf_beta_l(country_values, imf_table, code_table, beta_L) |>
    dplyr::select(-"country_input")

  dplyr::bind_cols(
    tibble::tibble(asset = as.character(assets$asset)),
    nace,
    imf
  ) |>
    dplyr::mutate(lp_loss = heat_lp_loss_for_exposure(.data$lp_exposure), .after = "lp_exposure")
}

#' Warnings describing fallbacks applied while building heat attributes (internal)
#' @noRd
global_heat_attribute_warnings <- function(attrs, excess_missing_assets = character(0)) {
  msgs <- character(0)
  dflt <- attrs$asset[attrs$nace_match == "default"]
  if (length(dflt) > 0) {
    msgs <- c(msgs, paste0(
      "[heat] No NACE match for sector of asset(s) ", paste(dflt, collapse = ", "),
      "; labour-productivity exposure defaulted to ", HEAT_DEFAULT_EXPOSURE, "."
    ))
  }
  dflt_beta <- attrs$asset[attrs$beta_L_source == HEAT_BETA_SOURCE_DEFAULT]
  if (length(dflt_beta) > 0) {
    msgs <- c(msgs, paste0(
      "[heat] Country not found in the IMF classification for asset(s) ", paste(dflt_beta, collapse = ", "),
      "; classified as Emerging Market and Developing Economies (beta_L_source = ", HEAT_BETA_SOURCE_DEFAULT, ")."
    ))
  }
  if (length(excess_missing_assets) > 0) {
    msgs <- c(msgs, paste0(
      "[heat] No heat-map value (missing coordinates) for asset(s) ",
      paste(excess_missing_assets, collapse = ", "), "; no heat shock applied."
    ))
  }
  msgs
}

#' Build deterministic Heat event rows (one per asset x event) for the shock engine (internal)
#'
#' @description Output rows carry everything the config's revenue equation needs
#'   (`output_change_pct`, from `s` and `beta_L`) plus display columns
#'   (NACE match, exposure class, statistic, scenario). They are bound to the
#'   other hazards' factor rows in `compute_risk()` and flow through the unmodified
#'   `compute_shock_trajectories()` machinery.
#' @param assets assets already filtered to portfolio companies
#' @param events_heat heat event rows (event_id, hazard_type, hazard_name, scenario_name,
#'   return_period, event_year, statistic)
#' @param hazard_configs named list of configs (must include the global heat config)
#' @param hazards_dir `.../hazards/config_global`
#' @param spatial_separation_data,base_dir used to resolve the events' administrative-region
#'   selection (`global_adm` scheme); `spatial_separation_data` may be NULL, it is then loaded
#'   from `base_dir` when an event has a regional selection
#' @return list(rows = tibble, warnings = character). `rows` has one row per asset and event;
#'   assets outside an event's selected regions keep `spatial_included = FALSE` and have no
#'   heat values (the caller must not shock them).
#' @noRd
build_global_heat_event_factors <- function(assets, events_heat, hazard_configs, hazards_dir,
                                            spatial_separation_data = NULL, base_dir = NULL) {
  if (is.null(events_heat) || nrow(events_heat) == 0) {
    return(list(rows = tibble::tibble(), warnings = character(0)))
  }
  mappings_dir <- file.path(dirname(hazards_dir), "mappings")
  attrs <- build_global_heat_asset_attributes(
    assets, mappings_dir, hazard_configs[[as.character(events_heat$hazard_type[[1]])]]
  )

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
  for (i in seq_len(nrow(events_heat))) {
    ev <- events_heat[i, ]
    hazard_config <- hazard_configs[[as.character(ev$hazard_type)]]
    maps_dir <- global_heat_maps_dir(hazards_dir, hazard_config)
    statistic <- if ("statistic" %in% names(ev) && !is.na(ev$statistic) && nzchar(as.character(ev$statistic))) {
      as.character(ev$statistic)
    } else {
      "median"
    }
    year <- as.numeric(ev$event_year)
    rp <- as.numeric(ev$return_period)

    excess_long <- extract_global_heat_excess(
      asset_meta, maps_dir, ssp = as.character(ev$scenario_name),
      statistic = statistic, return_periods = rp,
      needed_centres = NULL
    )
    blended <- blend_global_heat_excess(excess_long, year)
    w <- heat_blend_weights(year, unique(excess_long$centre_year))
    missing_assets <- union(missing_assets, blended$asset[is.na(blended$excess_days)])

    rows[[i]] <- asset_meta |>
      dplyr::left_join(
        blended |> dplyr::select("asset", excess_heat_days = "excess_days"),
        by = "asset"
      ) |>
      dplyr::mutate(
        excess_heat_days = dplyr::coalesce(.data$excess_heat_days, 0),
        s = heat_labour_shock(.data$excess_heat_days, .data$lp_loss),
        output_change_pct = heat_output_change_pct(.data$s, .data$beta_L),
        event_id = as.character(ev$event_id),
        hazard_type = as.character(ev$hazard_type),
        hazard_name = as.character(ev$hazard_name),
        scenario_name = as.character(ev$scenario_name),
        hazard_return_period = rp,
        event_year = as.integer(year),
        statistic = statistic,
        heat_map_weights = paste0(w$centre_year, ":", round(w$weight, 4), collapse = "; "),
        spatial_included = TRUE,
        spatial_multiplier = 1,
        spatial_exposure_status = NA_character_,
        matching_method = "coordinates"
      )
  }

  rows <- dplyr::bind_rows(rows)
  rows <- apply_heat_spatial_selection(rows, events_heat, hazard_configs, spatial_separation_data, base_dir)

  list(
    rows = rows,
    warnings = global_heat_attribute_warnings(attrs, missing_assets)
  )
}

#' Apply each heat event's regional selection to its asset rows (internal)
#'
#' @description Reuses `evaluate_spatial_separation()` (point-in-polygon on the asset
#'   coordinates for the `global_adm` scheme). Assets outside the selected regions are marked
#'   `spatial_included = FALSE` / `spatial_multiplier = 0` with the usual status text and their
#'   heat values (`excess_heat_days`, `s`, `output_change_pct`) are blanked, mirroring how
#'   excluded flood rows are reported. Events without a regional selection (whole area) are
#'   returned unchanged.
#' @noRd
apply_heat_spatial_selection <- function(rows, events_heat, hazard_configs, spatial_separation_data, base_dir) {
  apply_global_adm_spatial_selection(
    rows, events_heat, hazard_configs, spatial_separation_data, base_dir,
    value_cols = c("excess_heat_days", "s", "output_change_pct"),
    label = "heat"
  )
}

#' Apply each event's administrative-region selection to its asset rows (internal)
#'
#' @description Shared by the Global Heat and Global Drought engines (`global_adm` scheme).
#'   Assets outside the selected regions get `spatial_included = FALSE` /
#'   `spatial_multiplier = 0` and their hazard `value_cols` are blanked; whole-area events are
#'   returned unchanged.
#' @param value_cols hazard value columns to blank for assets outside the selection
#' @param label hazard label used in console messages
#' @noRd
apply_global_adm_spatial_selection <- function(rows, events_heat, hazard_configs, spatial_separation_data, base_dir,
                                               value_cols, label = "heat") {
  if (nrow(rows) == 0) {
    return(rows)
  }
  if (!"spatial_level" %in% names(events_heat)) {
    return(rows)
  }
  # Only the administrative levels apply to heat; any other level (e.g. a basin level from a
  # hand-edited events file) is not a regional selection for heat and covers the whole area.
  lvl <- tolower(as.character(events_heat$spatial_level))
  not_adm <- !is.na(lvl) & nzchar(lvl) & !lvl %in% c(whole_area_spatial_levels(), names(GLOBAL_ADM_LEVELS))
  if (any(not_adm)) {
    message(
      "[", label, "] Ignoring the non-administrative spatial level '", paste(unique(lvl[not_adm]), collapse = "', '"),
      "' on ", tools::toTitleCase(label), " event(s) ", paste(events_heat$event_id[not_adm], collapse = ", "),
      " - ", tools::toTitleCase(label), " covers the whole area unless a country / ADM1 / ADM2 selection is made."
    )
    events_heat$spatial_level[not_adm] <- GLOBAL_WHOLE_AREA_LEVEL
    lvl[not_adm] <- GLOBAL_WHOLE_AREA_LEVEL
  }
  if (all(is.na(lvl) | !nzchar(lvl) | lvl %in% whole_area_spatial_levels())) {
    return(rows)
  }

  spatial_eval <- evaluate_spatial_separation(
    assets_with_events = rows |>
      dplyr::select(dplyr::any_of(c("asset", "company", "latitude", "longitude", "event_id"))),
    events = events_heat,
    hazard_configs = hazard_configs,
    spatial_separation_data = spatial_separation_data,
    base_dir = base_dir
  ) |>
    dplyr::select("asset", "event_id", "spatial_included", "spatial_multiplier", "spatial_exposure_status")

  rows <- rows |>
    dplyr::select(-dplyr::any_of(c("spatial_included", "spatial_multiplier", "spatial_exposure_status"))) |>
    dplyr::left_join(spatial_eval, by = c("asset", "event_id")) |>
    dplyr::mutate(
      spatial_included = dplyr::coalesce(.data$spatial_included, TRUE),
      spatial_multiplier = dplyr::coalesce(as.numeric(.data$spatial_multiplier), 1)
    )

  outside <- !rows$spatial_included
  for (col in intersect(value_cols, names(rows))) {
    rows[[col]][outside] <- NA_real_
  }
  rows
}

# ---------------------------------------------------------------------------
# Probabilistic (AAL) heat
# ---------------------------------------------------------------------------

#' Heat revenue-loss rate per (asset, return period) for one year (internal)
#'
#' @description Reuses `evaluate_hazard_shock()` with the config's own, unmodified
#'   revenue equation and a unit revenue, so `1 - shock_value` IS the loss rate --
#'   exactly the pattern `compute_aal_channels()` uses for flood.
#' @param excess_by_rp tibble(asset, return_period, excess_days) for one year
#' @param attrs per-asset heat attributes (`build_global_heat_asset_attributes()`)
#' @noRd
heat_loss_rates_by_rp <- function(excess_by_rp, attrs, hazard_config) {
  rps <- sort(unique(excess_by_rp$return_period))
  res <- vector("list", length(rps))
  for (k in seq_along(rps)) {
    d <- excess_by_rp[excess_by_rp$return_period == rps[[k]], , drop = FALSE] |>
      dplyr::select("asset", excess_heat_days = "excess_days") |>
      dplyr::mutate(excess_heat_days = dplyr::coalesce(.data$excess_heat_days, 0)) |>
      dplyr::left_join(attrs |> dplyr::select("asset", "lp_loss", "beta_L"), by = "asset") |>
      dplyr::mutate(
        s = heat_labour_shock(.data$excess_heat_days, .data$lp_loss),
        output_change_pct = heat_output_change_pct(.data$s, .data$beta_L),
        revenue = 1
      )
    sh <- evaluate_hazard_shock(d, hazard_config, "revenue", combine = "error")
    res[[k]] <- tibble::tibble(
      asset = sh$asset, return_period = rps[[k]],
      s = d$s[match(sh$asset, d$asset)],
      output_change_pct = d$output_change_pct[match(sh$asset, d$asset)],
      loss = 1 - sh$shock_value
    )
  }
  dplyr::bind_rows(res)
}

#' Revenue adjustment for deterministic heat rows: baseline revenue x output change (internal)
#'
#' @description `revenue_adjustment = baseline_revenue x output_change_pct`, using each asset's
#'   baseline revenue in the event year. Applied to the heat rows of `assets_factors` (output
#'   only; the shock itself is `revenue * (1 + output_change_pct)`, i.e. baseline + this adjustment).
#' @param assets_factors tibble from `compute_risk()` (flood/other rows and heat rows)
#' @param yearly_baseline tibble(asset, year, revenue) from `compute_baseline_trajectories()`
#' @noRd
add_heat_revenue_adjustment <- function(assets_factors, yearly_baseline) {
  if (is.null(assets_factors) || nrow(assets_factors) == 0 || !"output_change_pct" %in% names(assets_factors)) {
    return(assets_factors)
  }
  base <- yearly_baseline |>
    dplyr::transmute(
      asset = as.character(.data$asset), event_year = as.integer(.data$year),
      baseline_revenue = .data$revenue
    ) |>
    dplyr::distinct(.data$asset, .data$event_year, .keep_all = TRUE)
  assets_factors |>
    dplyr::mutate(asset = as.character(.data$asset), event_year = as.integer(.data$event_year)) |>
    dplyr::left_join(base, by = c("asset", "event_year")) |>
    dplyr::mutate(
      baseline_revenue = dplyr::if_else(is.na(.data$output_change_pct), NA_real_, .data$baseline_revenue),
      revenue_adjustment = .data$baseline_revenue * .data$output_change_pct
    )
}

#' Add the revenue adjustment to the yearly heat AAL (probabilistic) (internal)
#'
#' @description The heat AAL rate is the expected annual LOSS fraction of revenue, i.e. minus the
#'   expected `output_change_pct`. The average annual revenue adjustment is therefore, for each
#'   year, `baseline_revenue x (-AAL rate)` (negative = revenue lost), averaged over the horizon.
#' @param heat_aal list from `compute_global_heat_aal()`
#' @param yearly_baseline tibble(asset, year, revenue)
#' @return `heat_aal` with `yearly$revenue_adjustment`, `yearly$baseline_revenue` and
#'   `summary$avg_annual_revenue_adjustment` added
#' @noRd
add_heat_aal_revenue_adjustment <- function(heat_aal, yearly_baseline) {
  base <- yearly_baseline |>
    dplyr::transmute(asset = as.character(.data$asset), year = as.integer(.data$year), baseline_revenue = .data$revenue) |>
    dplyr::distinct(.data$asset, .data$year, .keep_all = TRUE)
  heat_aal$yearly <- heat_aal$yearly |>
    dplyr::mutate(asset = as.character(.data$asset), year = as.integer(.data$year)) |>
    dplyr::left_join(base, by = c("asset", "year")) |>
    dplyr::mutate(revenue_adjustment = -.data$baseline_revenue * .data$aal_lp_revenue_rate)
  avg <- heat_aal$yearly |>
    dplyr::group_by(.data$asset) |>
    dplyr::summarise(avg_annual_revenue_adjustment = mean(.data$revenue_adjustment), .groups = "drop")
  heat_aal$summary <- heat_aal$summary |>
    dplyr::left_join(avg, by = "asset")
  heat_aal
}

#' Heat AAL for every year of the projection horizon (internal)
#'
#' @description For each year, the heat maps are blended to that year's climate
#'   (see `heat_blend_weights()`), the revenue-loss rate is evaluated at every
#'   available return period, and the loss curve is integrated into an Average
#'   Annual Loss with `calculate_aal()`. Heat has no "no-loss" return period
#'   (excess days exist in almost every year), so the curve is extended flat
#'   from the smallest return period down to RP = 1 instead of assuming zero
#'   loss at RP = 1 (which would understate heat AAL); above the largest return
#'   period `calculate_aal()` already holds the loss flat.
#' @param assets assets filtered to portfolio companies
#' @param event one probabilistic Heat event (scenario_name, statistic)
#' @param hazard_configs named list of configs
#' @param hazards_dir `.../hazards/config_global`
#' @param years integer vector of projection years
#' @return list(summary = tibble(asset, aal_lp_revenue_rate = mean over years),
#'   yearly = tibble(asset, year, aal_lp_revenue_rate),
#'   detail = tibble(asset, return_period, channel, loss, aep) (mean loss over years),
#'   factors_by_rp = tibble (per asset x return period diagnostics: horizon-mean excess days,
#'   labour shock s and output change),
#'   warnings = character)
#' @noRd
compute_global_heat_aal <- function(assets, event, hazard_configs, hazards_dir, years) {
  hazard_config <- hazard_configs[[as.character(event$hazard_type[[1]])]]
  maps_dir <- global_heat_maps_dir(hazards_dir, hazard_config)
  mappings_dir <- file.path(dirname(hazards_dir), "mappings")
  attrs <- build_global_heat_asset_attributes(assets, mappings_dir, hazard_config)

  statistic <- if ("statistic" %in% names(event) && !is.na(event$statistic[[1]]) && nzchar(as.character(event$statistic[[1]]))) {
    as.character(event$statistic[[1]])
  } else {
    "median"
  }

  asset_pts <- assets |>
    dplyr::mutate(asset = as.character(.data$asset)) |>
    dplyr::distinct(.data$asset, .keep_all = TRUE)
  excess_long <- extract_global_heat_excess(
    asset_pts, maps_dir, ssp = as.character(event$scenario_name[[1]]), statistic = statistic
  )
  missing_assets <- unique(excess_long$asset[is.na(excess_long$excess_days)])

  years <- sort(unique(as.integer(years)))
  yearly <- vector("list", length(years))
  detail_by_year <- vector("list", length(years))
  excess_by_year <- vector("list", length(years))
  for (k in seq_along(years)) {
    blended <- blend_global_heat_excess(excess_long, years[[k]])
    losses <- heat_loss_rates_by_rp(blended, attrs, hazard_config)
    excess_by_year[[k]] <- blended
    detail_by_year[[k]] <- losses |> dplyr::mutate(year = years[[k]])

    yearly[[k]] <- losses |>
      dplyr::group_by(.data$asset) |>
      dplyr::summarise(
        aal_lp_revenue_rate = {
          o <- order(.data$return_period)
          rp <- .data$return_period[o]
          ls <- .data$loss[o]
          calculate_aal(c(1, rp), c(ls[[1]], ls))
        },
        .groups = "drop"
      ) |>
      dplyr::mutate(year = years[[k]])
  }
  yearly <- dplyr::bind_rows(yearly)
  detail_all <- dplyr::bind_rows(detail_by_year)

  summary <- yearly |>
    dplyr::group_by(.data$asset) |>
    dplyr::summarise(aal_lp_revenue_rate = mean(.data$aal_lp_revenue_rate), .groups = "drop")

  detail <- detail_all |>
    dplyr::group_by(.data$asset, .data$return_period) |>
    dplyr::summarise(loss = mean(.data$loss), .groups = "drop") |>
    dplyr::mutate(channel = "aal_lp_revenue_rate", aep = 1 / .data$return_period) |>
    dplyr::select("asset", "return_period", "channel", "loss", "aep") |>
    dplyr::arrange(.data$asset, .data$return_period)

  excess_mean <- dplyr::bind_rows(excess_by_year) |>
    dplyr::group_by(.data$asset, .data$return_period) |>
    dplyr::summarise(excess_heat_days = mean(.data$excess_days), .groups = "drop") |>
    dplyr::left_join(
      detail_all |>
        dplyr::group_by(.data$asset, .data$return_period) |>
        dplyr::summarise(s = mean(.data$s), output_change_pct = mean(.data$output_change_pct), .groups = "drop"),
      by = c("asset", "return_period")
    )

  meta_cols <- intersect(
    c("asset", "company", "latitude", "longitude", "country", "asset_category", "asset_subtype",
      "share_of_economic_activity"),
    names(asset_pts)
  )
  factors_by_rp <- asset_pts |>
    dplyr::select(dplyr::all_of(meta_cols)) |>
    dplyr::left_join(attrs, by = "asset") |>
    dplyr::inner_join(excess_mean, by = "asset") |>
    dplyr::mutate(
      hazard_type = as.character(event$hazard_type[[1]]),
      scenario_name = as.character(event$scenario_name[[1]]),
      statistic = statistic
    ) |>
    dplyr::arrange(.data$asset, .data$return_period)

  list(
    summary = summary,
    yearly = yearly,
    detail = detail,
    factors_by_rp = factors_by_rp,
    warnings = global_heat_attribute_warnings(attrs, missing_assets)
  )
}
