#' Read a TRISK company_trajectories output file (CSV or Excel)
#'
#' @title Read TRISK asset-level profit pathway output
#' @description Reads TRISK's asset-level transition-risk profit pathway output
#'   (`company_trajectories.csv` or `.xlsx`). Every column present in the file is
#'   retained (`run_id`, `asset_name`, `company_name`, `country_iso2`, `sector`,
#'   `technology`, ...), not just the ones the physical-risk adjustment calculation
#'   needs, so they remain available for display/filtering in the UI.
#'
#'   The file may include a trailing year (e.g. 2051) per asset -- only years
#'   2026-2050 form the real profit pathway used anywhere downstream, so any row
#'   outside that range is dropped here rather than silently propagating.
#'
#'   Profit columns commonly arrive as strings like `" 700,000.00 "` (comma
#'   thousands separator, dot decimal separator). This is parsed explicitly and is
#'   *not* routed through the comma-decimal locale convention `read_csv2()` assumes
#'   for the existing Brazilian-locale semicolon input files -- that convention
#'   would misparse these values.
#' @param file_path Path to a `company_trajectories.csv` or `.xlsx` file
#' @return tibble with (at least) columns: run_id, asset_id, asset_name, company_id,
#'   company_name, country_iso2, sector, technology, year,
#'   net_profits_baseline_scenario, net_profits_shock_scenario -- filtered to
#'   year %in% 2026:2050
#' @export
read_trisk_output <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("TRISK output file not found at: ", file_path)
  }

  is_csv <- grepl("\\.csv$", file_path, ignore.case = TRUE)
  is_xlsx <- grepl("\\.xlsx?$", file_path, ignore.case = TRUE)
  if (!is_csv && !is_xlsx) {
    stop("Unsupported TRISK output file type (expected .csv or .xlsx): ", file_path)
  }

  message("[read_trisk_output] Reading TRISK output from: ", file_path)

  if (is_csv) {
    separator <- detect_csv_separator(file_path)
    delim <- if (separator == "semicolon") ";" else ","
    trisk_raw <- readr::read_delim(
      file_path,
      delim = delim,
      col_types = readr::cols(.default = "c"),
      show_col_types = FALSE,
      locale = readr::locale(encoding = "UTF-8")
    ) |>
      tibble::as_tibble()
  } else {
    trisk_raw <- readxl::read_excel(file_path) |>
      tibble::as_tibble() |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  }

  # Drop unnamed trailing columns that spreadsheet exports add (e.g. from trailing
  # delimiters in the header row) -- same pattern used for the legacy company file
  # in read_companies().
  trisk_raw <- trisk_raw |>
    dplyr::select(-dplyr::matches("^(\\.\\.\\.)?\\d+$"))

  required_cols <- c(
    "asset_id", "company_id", "year",
    "net_profits_baseline_scenario", "net_profits_shock_scenario"
  )
  missing_cols <- setdiff(required_cols, names(trisk_raw))
  if (length(missing_cols) > 0) {
    stop(
      "TRISK output file is missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # A lone "-" is accounting notation for zero (seen in the real TRISK export,
  # e.g. a company whose shock-scenario profit is exactly 0 in a given year) and
  # is distinct from a genuinely blank cell (e.g. the excluded 2051 rows) -- the
  # former must become 0, not NA, or a real zero-profit result would be silently
  # dropped from the pathway.
  clean_number <- function(x) {
    x_clean <- trimws(as.character(x))
    x_clean <- gsub(",", "", x_clean, fixed = TRUE)
    x_clean[x_clean == "-"] <- "0"
    x_clean[x_clean == ""] <- NA_character_
    suppressWarnings(as.numeric(x_clean))
  }

  trisk_clean <- trisk_raw |>
    dplyr::mutate(
      year = suppressWarnings(as.integer(trimws(as.character(.data$year)))),
      net_profits_baseline_scenario = clean_number(.data$net_profits_baseline_scenario),
      net_profits_shock_scenario = clean_number(.data$net_profits_shock_scenario),
      asset_id = trimws(as.character(.data$asset_id)),
      company_id = trimws(as.character(.data$company_id))
    ) |>
    dplyr::filter(!is.na(.data$asset_id) & nzchar(.data$asset_id))

  n_before <- nrow(trisk_clean)
  trisk_clean <- trisk_clean |>
    dplyr::filter(!is.na(.data$year), .data$year >= 2026, .data$year <= 2050)
  n_dropped <- n_before - nrow(trisk_clean)
  if (n_dropped > 0) {
    message("[read_trisk_output] Dropped ", n_dropped, " row(s) outside the 2026-2050 pathway range")
  }

  message("[read_trisk_output] Loaded ", nrow(trisk_clean), " asset-year rows")
  trisk_clean
}

#' Join a TRISK output table to PRISK asset inputs
#'
#' @title Join TRISK profit pathways to PRISK asset attributes
#' @description Joins TRISK's asset-level profit pathways to the PRISK asset input
#'   (from `read_assets()`) on the compound key `asset_id + company_id` (TRISK) ==
#'   `asset + company` (PRISK), so downstream physical-risk adjustment has access to
#'   each asset's `asset_category`, `cnae`, location, etc. Adds `asset`/`company`
#'   alias columns (equal to `asset_id`/`company_id` for matched rows) so the joined
#'   result is directly usable by every existing PRISK function that expects those
#'   column names, while keeping the original TRISK metadata columns intact.
#'
#'   Any TRISK asset_id/company_id combination with no matching PRISK asset is
#'   excluded from the result and reported via `warning()`, not silently dropped
#'   without a trace -- a TRISK asset that can't be located/typed in PRISK cannot be
#'   physical-risk-adjusted.
#' @param trisk_df tibble from `read_trisk_output()`
#' @param prisk_assets tibble from `read_assets()`
#' @return tibble: `trisk_df`'s columns, joined with `prisk_assets`' columns, plus
#'   `asset`/`company` alias columns
#' @export
join_trisk_to_prisk_assets <- function(trisk_df, prisk_assets) {
  if (is.null(trisk_df) || nrow(trisk_df) == 0) {
    stop("join_trisk_to_prisk_assets: trisk_df is empty")
  }
  if (is.null(prisk_assets) || nrow(prisk_assets) == 0) {
    stop("join_trisk_to_prisk_assets: prisk_assets is empty")
  }

  trisk_df <- trisk_df |>
    dplyr::mutate(
      asset_id = trimws(as.character(.data$asset_id)),
      company_id = trimws(as.character(.data$company_id))
    )
  prisk_keyed <- prisk_assets |>
    dplyr::mutate(
      asset = trimws(as.character(.data$asset)),
      company = trimws(as.character(.data$company))
    )

  # PRISK's asset file also has sector/technology/asset_name columns, which would
  # otherwise collide with TRISK's and get suffixed. TRISK's own values are the
  # ones that matter for display in this mode; PRISK's asset_category/cnae/location
  # (needed for the AAL channel calculation) are unaffected and kept.
  prisk_keyed <- prisk_keyed |>
    dplyr::select(-dplyr::any_of(c("sector", "technology", "asset_name")))

  joined <- trisk_df |>
    dplyr::inner_join(
      prisk_keyed,
      by = c("asset_id" = "asset", "company_id" = "company")
    ) |>
    dplyr::mutate(asset = .data$asset_id, company = .data$company_id)

  unmatched <- trisk_df |>
    dplyr::anti_join(prisk_keyed, by = c("asset_id" = "asset", "company_id" = "company")) |>
    dplyr::distinct(.data$asset_id, .data$company_id)

  if (nrow(unmatched) > 0) {
    warning(
      "[join_trisk_to_prisk_assets] ", nrow(unmatched),
      " TRISK asset_id/company_id combination(s) have no matching PRISK asset and will be excluded: ",
      paste(utils::head(paste0(unmatched$asset_id, " (", unmatched$company_id, ")"), 10), collapse = ", "),
      if (nrow(unmatched) > 10) "..." else "",
      call. = FALSE
    )
  }

  if (nrow(joined) == 0) {
    stop(
      "join_trisk_to_prisk_assets: no TRISK rows matched any PRISK asset ",
      "(check that asset_id/company_id in the TRISK file match Asset id/ID in the PRISK asset file)"
    )
  }

  message("[join_trisk_to_prisk_assets] Joined ", nrow(joined), " TRISK asset-year rows to PRISK asset attributes")
  joined
}
