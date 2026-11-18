#' Compute per-asset, per-year interpolated AAL for the TRISK-PRISK integration mode
#'
#' @title Interpolated AAL pathways (present -> RCP8.5 / present -> RCP2.6)
#' @description For the TRISK-PRISK integration mode, physical-risk loss isn't a
#'   single flat AAL (as in the Brazil/Global Probabilistic mode) -- it grows over
#'   the pathway as the climate trajectory diverges from present-day conditions.
#'   This computes three flat, per-asset, per-channel AAL sets by reusing
#'   `extract_flood_depth_by_rp()` + `compute_aal_channels()` unchanged, once each
#'   for the `present`, `rcp26`, and `rcp85` scenarios, then linearly interpolates
#'   between the present-day anchor and the relevant endpoint for every year in
#'   `years`:
#'   \itemize{
#'     \item Baseline pathway: interpolates from `present` toward `rcp85`,
#'       anchored at `endpoint_year` (i.e. `AAL(t) = AAL_present +
#'       (t - anchor_year) / (endpoint_year - anchor_year) * (AAL_rcp85 - AAL_present)`).
#'     \item Shock pathway: the same formula, interpolating toward `rcp26` instead.
#'   }
#'   Applied separately to each of the three channels `compute_aal_channels()`
#'   already distinguishes (`aal_pd_profit`, `aal_pd_revenue_rate`,
#'   `aal_bd_revenue_rate`) -- none of the channel-isolation logic is re-derived
#'   here, only the temporal interpolation around it.
#' @param assets tibble of PRISK asset data (asset, company, asset_category,
#'   location, cnae, ...) -- e.g. the joined TRISK-PRISK asset table, since only
#'   the PRISK attribute columns are used here
#' @param hazards,hazards_inventory,hazard_configs,hazards_dir,precomputed_hazards
#'   Same as `compute_risk_probabilistic()` -- loaded hazard data/config
#' @param years Integer vector of pathway years to interpolate for (default 2026:2050)
#' @param anchor_year Numeric, the present-day anchor year for interpolation (default 2026)
#' @param endpoint_year Numeric, the year the RCP8.5/RCP2.6 endpoint AAL values are
#'   anchored to (default 2075)
#' @param aggregation_method Character, passed through to hazard extraction (default "mean")
#' @return list with:
#'   - `by_year`: long tibble (asset, year, pathway ["baseline"/"shock"],
#'     aal_pd_profit, aal_pd_revenue_rate, aal_bd_revenue_rate) -- one row per
#'     asset per year per pathway, ready for `apply_trisk_prisk_adjustment()`.
#'   - `endpoints`: list(present, rcp26, rcp85), each the flat per-asset AAL
#'     summary tibble for that scenario -- the `rcp85`/`rcp26` entries are the
#'     full, non-interpolated endpoint values the terminal-value calculation needs.
#'   - `detail`: combined per-return-period loss series across all three
#'     scenarios (asset, return_period, channel, loss, aep, scenario_name), the
#'     audit trail behind every AAL figure.
#' @export
compute_trisk_aal_by_year <- function(
  assets,
  hazards,
  hazards_inventory,
  hazard_configs,
  hazards_dir,
  precomputed_hazards,
  years = 2026:2050,
  anchor_year = 2026,
  endpoint_year = 2075,
  aggregation_method = "mean"
) {
  if (is.null(hazard_configs) || !"Flood" %in% names(hazard_configs)) {
    stop("compute_trisk_aal_by_year: hazard_configs must include a 'Flood' entry")
  }

  channel_cols <- c("aal_pd_profit", "aal_pd_revenue_rate", "aal_bd_revenue_rate")

  extract_one <- function(scenario_name) {
    factors <- extract_flood_depth_by_rp(
      assets = assets, hazards = hazards, hazards_inventory = hazards_inventory,
      hazard_configs = hazard_configs, hazards_dir = hazards_dir,
      precomputed_hazards = precomputed_hazards, hazard_type = "Flood",
      scenario_name = scenario_name, aggregation_method = aggregation_method
    )
    compute_aal_channels(factors, hazard_configs[["Flood"]])
  }

  aal_present <- extract_one("present")
  aal_rcp26 <- extract_one("rcp26")
  aal_rcp85 <- extract_one("rcp85")

  # An asset can be missing from one scenario's summary (e.g. no hazard coverage
  # for that scenario at its location) -- align on the union of assets seen
  # across all three, filling missing channel values with 0. This mirrors
  # compute_aal_channels()'s own "no coverage = 0 AAL contribution" convention
  # rather than dropping the asset from the pathway entirely.
  all_assets <- unique(c(aal_present$summary$asset, aal_rcp26$summary$asset, aal_rcp85$summary$asset))
  fill_missing <- function(summary_df) {
    tibble::tibble(asset = all_assets) |>
      dplyr::left_join(summary_df, by = "asset") |>
      dplyr::mutate(dplyr::across(dplyr::all_of(channel_cols), ~ dplyr::coalesce(., 0)))
  }
  present_summary <- fill_missing(aal_present$summary)
  rcp26_summary <- fill_missing(aal_rcp26$summary)
  rcp85_summary <- fill_missing(aal_rcp85$summary)

  by_year <- interpolate_aal_pathways(
    present_summary = present_summary,
    rcp26_summary = rcp26_summary,
    rcp85_summary = rcp85_summary,
    years = years,
    anchor_year = anchor_year,
    endpoint_year = endpoint_year
  )

  list(
    by_year = by_year,
    endpoints = list(present = present_summary, rcp26 = rcp26_summary, rcp85 = rcp85_summary),
    detail = dplyr::bind_rows(
      dplyr::mutate(aal_present$detail, scenario_name = "present"),
      dplyr::mutate(aal_rcp26$detail, scenario_name = "rcp26"),
      dplyr::mutate(aal_rcp85$detail, scenario_name = "rcp85")
    )
  )
}

#' Linearly interpolate per-asset AAL channels from present-day toward two endpoints
#'
#' @title Interpolate AAL pathways between three flat scenario summaries
#' @description The pure interpolation step behind `compute_trisk_aal_by_year()`,
#'   separated out so it can be unit tested without real hazard raster data: given
#'   three already-computed flat per-asset AAL summaries (present, rcp26, rcp85 --
#'   each shaped like `compute_aal_channels()$summary`), builds the baseline
#'   (present -> rcp85) and shock (present -> rcp26) pathways for every year in
#'   `years`, per the formula
#'   `AAL(t) = AAL_present + (t - anchor_year) / (endpoint_year - anchor_year) * (AAL_endpoint - AAL_present)`,
#'   applied independently to each of the three AAL channel columns.
#' @param present_summary,rcp26_summary,rcp85_summary tibbles with columns asset,
#'   aal_pd_profit, aal_pd_revenue_rate, aal_bd_revenue_rate -- one row per asset,
#'   the same set of assets in each (see `compute_trisk_aal_by_year()`'s
#'   `fill_missing()` step, which guarantees this before calling here)
#' @param years Integer vector of pathway years to interpolate for
#' @param anchor_year Numeric, the present-day anchor year (weight = 0 here)
#' @param endpoint_year Numeric, the year the endpoint AAL values are anchored to
#'   (weight = 1 here)
#' @return long tibble: asset, year, pathway ("baseline"/"shock"), aal_pd_profit,
#'   aal_pd_revenue_rate, aal_bd_revenue_rate
#' @noRd
interpolate_aal_pathways <- function(
  present_summary,
  rcp26_summary,
  rcp85_summary,
  years,
  anchor_year,
  endpoint_year
) {
  channel_cols <- c("aal_pd_profit", "aal_pd_revenue_rate", "aal_bd_revenue_rate")

  interpolate_pathway <- function(endpoint_summary, pathway_label) {
    base <- present_summary |>
      dplyr::select("asset", dplyr::all_of(channel_cols)) |>
      dplyr::rename_with(~ paste0(., "_present"), dplyr::all_of(channel_cols)) |>
      dplyr::left_join(
        endpoint_summary |>
          dplyr::select("asset", dplyr::all_of(channel_cols)) |>
          dplyr::rename_with(~ paste0(., "_endpoint"), dplyr::all_of(channel_cols)),
        by = "asset"
      )

    tidyr::crossing(asset = base$asset, year = years) |>
      dplyr::left_join(base, by = "asset") |>
      dplyr::mutate(
        pathway = pathway_label,
        weight = (.data$year - anchor_year) / (endpoint_year - anchor_year),
        aal_pd_profit = .data$aal_pd_profit_present +
          .data$weight * (.data$aal_pd_profit_endpoint - .data$aal_pd_profit_present),
        aal_pd_revenue_rate = .data$aal_pd_revenue_rate_present +
          .data$weight * (.data$aal_pd_revenue_rate_endpoint - .data$aal_pd_revenue_rate_present),
        aal_bd_revenue_rate = .data$aal_bd_revenue_rate_present +
          .data$weight * (.data$aal_bd_revenue_rate_endpoint - .data$aal_bd_revenue_rate_present)
      ) |>
      dplyr::select("asset", "year", "pathway", dplyr::all_of(channel_cols))
  }

  dplyr::bind_rows(
    interpolate_pathway(rcp85_summary, "baseline"),
    interpolate_pathway(rcp26_summary, "shock")
  )
}
