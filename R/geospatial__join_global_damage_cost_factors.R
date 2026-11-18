#' Load the ISO2 country -> region crosswalk (internal)
#'
#' @description Reads a two-column `iso2, region` mapping table. Read with
#'   `na = character()` (no string treated as a missing-value sentinel) because
#'   Namibia's real ISO 3166-1 alpha-2 code is the literal string "NA", which
#'   readr's/read.csv's default NA-string handling would otherwise silently
#'   turn into a missing value.
#' @param mappings_dir Character directory containing mapping tables
#' @param mapping_file Character filename, e.g. "iso2_to_region.csv"
#' @return tibble with columns `iso2`, `region`
#' @noRd
load_iso2_region_mapping <- function(mappings_dir, mapping_file) {
  table_path <- file.path(mappings_dir, mapping_file)
  if (!file.exists(table_path)) {
    stop("ISO2-to-region mapping table not found: ", table_path)
  }

  ext <- tolower(tools::file_ext(table_path))
  region_df <- if (ext == "csv") {
    readr::read_csv(
      table_path,
      col_types = readr::cols(.default = "c"),
      na = character(),
      show_col_types = FALSE
    ) |> tibble::as_tibble()
  } else if (ext %in% c("xlsx", "xls")) {
    readxl::read_excel(table_path, na = character()) |> tibble::as_tibble()
  } else {
    stop("Unsupported ISO2-to-region mapping table extension: ", ext)
  }

  region_df |>
    dplyr::rename_with(to_snake_case) |>
    dplyr::transmute(
      iso2 = toupper(trimws(as.character(.data$iso2))),
      region = trimws(as.character(.data$region))
    ) |>
    dplyr::filter(nzchar(.data$iso2)) |>
    dplyr::distinct(.data$iso2, .keep_all = TRUE)
}

#' Load and pivot the global flood cost-factors table to long format (internal)
#'
#' @description The source file is wide -- one row per country, one column per
#'   asset_category (`commercial building`, `industrial building`,
#'   `residential building`) -- with a `Country` name column and an `Iso2` code
#'   column. Pivots to long format (`iso2`, `asset_category`, `cost_factor`) so
#'   it can be joined like any other asset-attribute mapping table. Agriculture
#'   has no cost_factor column in the source (cost_factor is only used for
#'   buildings downstream), so agriculture rows are never produced here.
#' @param mappings_dir Character directory containing mapping tables
#' @param mapping_file Character filename, e.g. "flood_cost_factors_global.xlsx"
#' @return tibble with columns `iso2`, `asset_category`, `cost_factor`
#' @noRd
load_global_cost_factors_long <- function(mappings_dir, mapping_file) {
  cost_wide <- read_hazard_mapping_table(mappings_dir, mapping_file)

  # Match column names case-insensitively (and trimmed) -- the source is a
  # user-maintained Excel file, and header casing has already changed once.
  actual_names <- names(cost_wide)
  normalized_names <- tolower(trimws(actual_names))

  expected_categories <- c("commercial building", "industrial building", "residential building")
  category_idx <- match(expected_categories, normalized_names)
  asset_category_cols <- actual_names[category_idx[!is.na(category_idx)]]
  matched_categories <- expected_categories[!is.na(category_idx)]

  if (length(asset_category_cols) == 0) {
    stop(
      "load_global_cost_factors_long: none of the expected asset-category columns ",
      "('commercial building', 'industrial building', 'residential building') ",
      "were found in ", mapping_file
    )
  }
  iso2_col <- actual_names[match("iso2", normalized_names)]
  if (is.na(iso2_col)) {
    stop("load_global_cost_factors_long: '", mapping_file, "' is missing an 'Iso2' column")
  }

  # Rename to the canonical lowercase labels used by the damage factors file
  # and downstream asset_category values, regardless of the source header casing.
  cost_wide <- cost_wide |>
    dplyr::rename(!!!stats::setNames(asset_category_cols, matched_categories))

  cost_long <- cost_wide |>
    dplyr::transmute(
      iso2 = toupper(trimws(as.character(.data[[iso2_col]]))),
      dplyr::across(dplyr::all_of(matched_categories), as.numeric)
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(matched_categories),
      names_to = "asset_category",
      values_to = "cost_factor"
    ) |>
    dplyr::filter(nzchar(.data$iso2))

  # The source file labels the United Kingdom "UK"; assets use the ISO 3166-1
  # standard "GB". Add a "GB" alias row for every "UK" row so either works.
  uk_alias <- cost_long |>
    dplyr::filter(.data$iso2 == "UK") |>
    dplyr::mutate(iso2 = "GB")

  dplyr::bind_rows(cost_long, uk_alias) |>
    dplyr::distinct(.data$iso2, .data$asset_category, .keep_all = TRUE)
}

#' Join global (country + region-aware) flood damage and cost factors (internal)
#'
#' @description Global-mode / TRISK-PRISK counterpart to `join_damage_cost_factors()`,
#'   used instead of it whenever `hazards_dir` is `.../hazards/config_global`
#'   (Brazil mode is completely untouched -- see the branch at the top of
#'   `join_damage_cost_factors()`). Unlike the Brazil mapping (a single flat
#'   `asset_category` + `flood_depth_cm` table), the global factors come from two
#'   files with a different shape each, requiring dedicated joining logic:
#'   \itemize{
#'     \item **Damage/business-disruption**: keyed by `asset_category`, `region`,
#'       and closest-match `flood_depth_cm`. `region` isn't an asset input --
#'       it's derived here from each asset's `country` (ISO2) via the
#'       `iso2_to_region` mapping table, falling back to `"Global"` for any
#'       unmapped/missing country. If an asset's own region has no row for its
#'       asset_category (the source data has some gaps, e.g. no
#'       Africa/commercial-building rows), falls back to the `"Global"` region's
#'       row for that asset_category; if even that is missing, damage_factor and
#'       business_disruption are left `NA` (a warning lists every affected
#'       asset_category/region combination).
#'     \item **Cost factor**: keyed by `asset_category` and country (ISO2) --
#'       joined against the pivoted-to-long `flood_cost_factors_global` table
#'       (see `load_global_cost_factors_long()`). No fallback: an unmatched
#'       country leaves `cost_factor` `NA` (warned), since the source has no
#'       country-agnostic default to fall back to.
#'   }
#' @param assets_with_hazards Data frame in long format with asset and hazard
#'   information, including `hazard_type`, `hazard_indicator`, `asset_category`,
#'   `country` (ISO2), and indicator-specific values
#' @param hazard_configs Named list from `load_hazards_and_inventory()$configs`
#' @param hazards_dir Character path to the `.../hazards/config_global` directory
#' @return Data frame with `damage_factor`, `cost_factor`, and
#'   `business_disruption` columns joined in, same shape as
#'   `join_damage_cost_factors()`'s output
#' @noRd
join_global_damage_cost_factors <- function(assets_with_hazards, hazard_configs, hazards_dir) {
  if (is.null(assets_with_hazards) || nrow(assets_with_hazards) == 0) {
    stop("No hazard assets provided for mapping joins")
  }
  if (is.null(hazard_configs) || !"Flood" %in% names(hazard_configs)) {
    stop("join_global_damage_cost_factors: Flood hazard config not found in hazard_configs")
  }

  mappings_dir <- file.path(dirname(hazards_dir), "mappings")
  if (!dir.exists(mappings_dir)) {
    stop("hazards mappings directory does not exist: ", mappings_dir)
  }

  hazard_config <- hazard_configs[["Flood"]]
  mapping_file <- function(key, default_file) {
    m <- hazard_config$mappings[[key]]
    if (!is.null(m) && !is.null(m$file)) m$file else default_file
  }

  flood_rows <- assets_with_hazards |> dplyr::filter(.data$hazard_type == "Flood")
  other_rows <- assets_with_hazards |> dplyr::filter(.data$hazard_type != "Flood")
  if (nrow(flood_rows) == 0) {
    stop("join_global_damage_cost_factors: no Flood hazard rows found (Global mode only configures Flood)")
  }

  base_table <- build_indicator_wide(flood_rows, hazard_config)

  required_cols <- c("asset", "asset_category", "country", "flood_depth_cm")
  missing_cols <- setdiff(required_cols, names(base_table))
  if (length(missing_cols) > 0) {
    stop(
      "join_global_damage_cost_factors: assets are missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }

  base_table <- base_table |>
    dplyr::mutate(
      asset_category = as.character(.data$asset_category),
      .iso2 = toupper(trimws(as.character(.data$country)))
    )

  # --- 1. Derive region from country (ISO2), falling back to "Global" ---
  region_map <- load_iso2_region_mapping(mappings_dir, mapping_file("iso2_region", "iso2_to_region.csv"))
  base_table <- base_table |>
    dplyr::left_join(region_map, by = c(".iso2" = "iso2"))

  unmapped <- base_table |>
    dplyr::filter(is.na(.data$region)) |>
    dplyr::distinct(.data$.iso2) |>
    dplyr::pull(.data$.iso2)
  if (length(unmapped) > 0) {
    known <- unmapped[!is.na(unmapped) & nzchar(unmapped)]
    parts <- character(0)
    if (length(known) > 0) parts <- c(parts, paste("ISO2 code(s):", paste(known, collapse = ", ")))
    if (any(is.na(unmapped) | !nzchar(unmapped))) parts <- c(parts, "asset(s) with no country code")
    message(
      "[join_global_damage_cost_factors] No region mapping found for ",
      paste(parts, collapse = "; "), ". Falling back to the 'Global' region for these assets."
    )
  }
  base_table$region[is.na(base_table$region)] <- "Global"

  # --- 2. Damage factor + business disruption: asset_category + region + closest flood_depth_cm ---
  damage_df <- read_hazard_mapping_table(mappings_dir, mapping_file("damage_factors_global", "flood_damage_factors_global.xlsx")) |>
    dplyr::rename_with(to_snake_case) |>
    dplyr::mutate(
      asset_category = as.character(.data$asset_category),
      region = as.character(.data$region),
      flood_depth_cm = as.numeric(.data$flood_depth_cm),
      damage_factor = as.numeric(.data$damage_factor),
      business_disruption = as.numeric(.data$business_disruption)
    )

  required_damage_cols <- c("asset_category", "flood_depth_cm", "region", "damage_factor", "business_disruption")
  missing_damage_cols <- setdiff(required_damage_cols, names(damage_df))
  if (length(missing_damage_cols) > 0) {
    stop(
      "join_global_damage_cost_factors: damage factors file is missing required column(s): ",
      paste(missing_damage_cols, collapse = ", ")
    )
  }

  base_table$flood_depth_cm_raw <- base_table$flood_depth_cm
  base_table <- apply_intensity_matching(base_table, damage_df, "flood_depth_cm", "closest")

  exact_damage <- damage_df |>
    dplyr::select(
      "asset_category", "region", "flood_depth_cm",
      damage_factor_exact = "damage_factor", business_disruption_exact = "business_disruption"
    )
  global_damage <- damage_df |>
    dplyr::filter(.data$region == "Global") |>
    dplyr::select(
      "asset_category", "flood_depth_cm",
      damage_factor_global = "damage_factor", business_disruption_global = "business_disruption"
    )

  base_table <- base_table |>
    dplyr::left_join(exact_damage, by = c("asset_category", "region", "flood_depth_cm")) |>
    dplyr::left_join(global_damage, by = c("asset_category", "flood_depth_cm")) |>
    dplyr::mutate(
      damage_factor = dplyr::coalesce(.data$damage_factor_exact, .data$damage_factor_global),
      business_disruption = dplyr::coalesce(.data$business_disruption_exact, .data$business_disruption_global)
    ) |>
    dplyr::select(-"damage_factor_exact", -"business_disruption_exact", -"damage_factor_global", -"business_disruption_global")

  # Only flag a genuine mapping-coverage gap -- i.e. a real flood_depth_cm was
  # extracted for this asset/event but no damage_factor/business_disruption row
  # exists for its asset_category/region even after the Global fallback. A
  # missing flood_depth_cm (no hazard extracted at this asset's location for
  # this event -- routine, e.g. an asset outside the flooded area) also leaves
  # damage_factor/business_disruption NA, but that's not a mapping gap and
  # shouldn't be reported as one.
  still_missing <- base_table |>
    dplyr::filter(
      !is.na(.data$flood_depth_cm),
      is.na(.data$damage_factor) | is.na(.data$business_disruption)
    ) |>
    dplyr::distinct(.data$asset_category, .data$region)
  if (nrow(still_missing) > 0) {
    message(
      "[join_global_damage_cost_factors] No damage/business-disruption data for asset_category/region combination(s): ",
      paste0(still_missing$asset_category, " / ", still_missing$region, collapse = "; "),
      ". These assets will have NA damage_factor and business_disruption."
    )
  }

  # --- 3. Cost factor: asset_category + country (ISO2), no fallback ---
  cost_long <- load_global_cost_factors_long(mappings_dir, mapping_file("cost_factors_global", "flood_cost_factors_global.xlsx"))

  base_table <- base_table |>
    dplyr::left_join(
      cost_long |> dplyr::rename(cost_factor_matched = "cost_factor"),
      by = c(".iso2" = "iso2", "asset_category")
    ) |>
    dplyr::mutate(cost_factor = .data$cost_factor_matched) |>
    dplyr::select(-"cost_factor_matched")

  missing_cost <- base_table |>
    dplyr::filter(
      .data$asset_category %in% c("commercial building", "industrial building", "residential building"),
      is.na(.data$cost_factor)
    ) |>
    dplyr::distinct(.data$.iso2)
  if (nrow(missing_cost) > 0) {
    message(
      "[join_global_damage_cost_factors] No cost factor found for country code(s): ",
      paste(dplyr::coalesce(missing_cost$.iso2, "(none)"), collapse = ", "),
      ". These assets will have NA cost_factor."
    )
  }

  base_table$.iso2 <- NULL

  dplyr::bind_rows(base_table, other_rows)
}
