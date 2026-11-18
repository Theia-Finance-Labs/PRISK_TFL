#' Compute Probabilistic (Average Annual Loss) Climate Risk Analysis
#'
#' @title Probabilistic AAL orchestrator -- parallel to compute_risk(), never calls it
#' @description The Probabilistic-mode counterpart to `compute_risk()`. Never calls
#'   or modifies `compute_risk()` or any of its single-event-year shock functions
#'   (`apply_acute_revenue_shock()`, `apply_acute_profit_shock()`,
#'   `compute_shock_trajectories()`) -- the deterministic path is completely
#'   untouched by this function's existence. Instead it computes an Average Annual
#'   Loss (AAL) by integrating flood damage across every available return period
#'   for one scenario (see `calculate_aal()`, `compute_aal_channels()`), and
#'   deducts that AAL from *every* year of the pathway (see
#'   `compute_aal_shock_trajectories()`) rather than a single event year.
#'
#'   Reuses, unchanged, every function `compute_risk()` also uses for the parts
#'   that don't depend on how the shock itself is computed: `filter_assets_by_companies()`,
#'   `compute_baseline_trajectories()`, `concatenate_baseline_and_shock()`,
#'   `discount_yearly_profits()`, `aggregate_assets_to_company()`,
#'   `compute_companies_financials()`. The adverse scenario is labeled `"shock"`
#'   (the same literal string the deterministic path uses), so
#'   `gather_and_pivot_results()` (inside `compute_companies_financials()`) and
#'   the result-display modules that pivot on that string need no changes.
#'
#'   Scope: at most one Probabilistic event PER HAZARD TYPE per run -- Flood, global
#'   Heat (labour productivity) and/or global Drought (crop yield). Mixing multiple climate scenarios of the
#'   same hazard (e.g. present + rcp85) in one summed AAL would have no coherent
#'   physical interpretation -- scenarios are alternative futures -- but distinct
#'   hazards are concurrent risks, so a Flood AAL and a Heat AAL add (as the
#'   deterministic path adds distinct hazard events). Flood's AAL is constant over
#'   the horizon; Heat's AAL is recomputed for every year from the blended maps
#'   (see `compute_global_heat_aal()`), and so is Drought's (all four seasons, see
#'   `compute_global_drought_aal()`). Spatial separation is not applied in
#'   probabilistic mode (whole selected area only).
#' @param assets tibble of asset data (from `read_assets()`)
#' @param companies tibble of company data (from `read_companies()`)
#' @param events tibble with at most one row per hazard type: hazard_type ("Flood"
#'   and/or the global heat hazard), scenario_name, (heat: statistic),
#'   calc_mode ("probabilistic")
#' @param hazards Named list of SpatRaster objects (from `load_hazards_and_inventory()`)
#' @param hazards_inventory tibble from `load_hazards_and_inventory()$inventory`
#' @param hazard_configs Named list from `load_hazards_and_inventory()$configs`
#' @param hazards_dir Character path to the hazards config directory
#' @param precomputed_hazards tibble from `read_precomputed_hazards()`
#' @param growth_rate Numeric. Revenue growth rate assumption (default: 0.02)
#' @param discount_rate Numeric. Discount rate for NPV calculation (default: 0.05)
#' @param risk_free_rate Numeric. Risk-free rate for Merton model (default: 0.02)
#' @param aggregation_method Character. Statistical aggregation method for hazard
#'   extraction (default: "mean")
#' @param on_progress Optional function(value, message) called at each pipeline step
#' @return List with the same shape `compute_risk()` returns (`assets_factors`,
#'   `companies`, `assets_yearly`, `companies_yearly`, `aggregation_method`), plus
#'   one extra field, `aal_channels` -- a list with `summary` (per-asset AAL by
#'   channel) and `detail` (the per-return-period loss series behind each
#'   channel's AAL) -- used by the Probabilistic-mode result displays.
#' @export
compute_risk_probabilistic <- function(
  assets,
  companies,
  events,
  hazards,
  hazards_inventory,
  hazard_configs,
  hazards_dir,
  precomputed_hazards,
  growth_rate = 0.02,
  discount_rate = 0.05,
  risk_free_rate = 0.02,
  aggregation_method = "mean",
  on_progress = NULL
) {
  .report_progress <- function(value, msg) {
    message(sprintf("[compute_risk_probabilistic] %3d%%  %s", round(value * 100), msg))
    if (!is.null(on_progress)) {
      tryCatch(on_progress(value, msg), error = function(e) message("[progress callback error] ", conditionMessage(e)))
    }
  }

  if (!is.data.frame(assets) || nrow(assets) == 0) {
    stop("compute_risk_probabilistic: assets must be a non-empty data.frame")
  }
  if (!is.data.frame(companies) || nrow(companies) == 0) {
    stop("compute_risk_probabilistic: companies must be a non-empty data.frame")
  }
  if (!is.data.frame(events) || nrow(events) == 0) {
    stop("compute_risk_probabilistic: events must be a non-empty data.frame")
  }
  if ("calc_mode" %in% names(events) && !all(events$calc_mode == "probabilistic")) {
    stop("compute_risk_probabilistic: all events must have calc_mode == 'probabilistic'")
  }
  hazards_inventory <- drop_heat_ui_inventory_columns(hazards_inventory)

  heat_types <- global_heat_hazard_types(hazard_configs)
  drought_types <- global_drought_hazard_types(hazard_configs)
  supported_types <- c("Flood", heat_types, drought_types)
  if (!all(events$hazard_type %in% supported_types)) {
    stop(
      "compute_risk_probabilistic: only hazard types ", paste(supported_types, collapse = ", "),
      " are supported in this version"
    )
  }
  if (anyDuplicated(events$hazard_type) > 0) {
    stop(
      "compute_risk_probabilistic: only one Probabilistic (AAL) event is supported per hazard type ",
      "in this version -- remove extra events. Climate scenarios are alternative futures, ",
      "not concurrent risks to sum."
    )
  }
  flood_events <- events[events$hazard_type == "Flood", , drop = FALSE]
  heat_events <- events[events$hazard_type %in% heat_types, , drop = FALSE]
  drought_events <- events[events$hazard_type %in% drought_types, , drop = FALSE]
  has_flood <- nrow(flood_events) > 0
  has_heat <- nrow(heat_events) > 0
  has_drought <- nrow(drought_events) > 0
  if (has_flood && (is.null(hazard_configs) || !"Flood" %in% names(hazard_configs))) {
    stop("compute_risk_probabilistic: hazard_configs must include a 'Flood' entry")
  }

  .report_progress(0.05, "Preparing inputs...")
  assets <- filter_assets_by_companies(assets, companies)

  assets_factors_by_rp <- tibble::tibble()
  aal <- list(
    summary = tibble::tibble(
      asset = unique(as.character(assets$asset)),
      aal_pd_profit = 0, aal_pd_revenue_rate = 0, aal_bd_revenue_rate = 0
    ),
    detail = tibble::tibble(
      asset = character(), return_period = numeric(), channel = character(),
      loss = numeric(), aep = numeric()
    )
  )

  if (has_flood) {
    hazard_type <- flood_events$hazard_type[[1]]
    scenario_name <- flood_events$scenario_name[[1]]

    .report_progress(0.20, "Extracting hazard statistics for all return periods...")
    assets_factors_by_rp <- extract_flood_depth_by_rp(
      assets = assets,
      hazards = hazards,
      hazards_inventory = hazards_inventory,
      hazard_configs = hazard_configs,
      hazards_dir = hazards_dir,
      precomputed_hazards = precomputed_hazards,
      hazard_type = hazard_type,
      scenario_name = scenario_name,
      aggregation_method = aggregation_method
    )

    .report_progress(0.45, "Computing Average Annual Loss channels...")
    aal <- compute_aal_channels(assets_factors_by_rp, hazard_configs[[hazard_type]])
  }

  .report_progress(0.60, "Computing baseline revenue trajectories...")
  yearly_baseline <- compute_baseline_trajectories(
    baseline_assets = assets,
    companies = companies,
    growth_rate = growth_rate
  )

  heat_aal <- NULL
  if (has_heat) {
    .report_progress(0.65, "Computing heat Average Annual Loss for every year...")
    heat_aal <- compute_global_heat_aal(
      assets = assets,
      event = heat_events[1, , drop = FALSE],
      hazard_configs = hazard_configs,
      hazards_dir = hazards_dir,
      years = unique(yearly_baseline$year)
    )
    for (msg in heat_aal$warnings) message(msg)
    heat_aal <- add_heat_aal_revenue_adjustment(heat_aal, yearly_baseline)

    aal$summary <- aal$summary |>
      dplyr::left_join(heat_aal$summary, by = "asset") |>
      dplyr::mutate(aal_lp_revenue_rate = dplyr::coalesce(.data$aal_lp_revenue_rate, 0))
    aal$detail <- dplyr::bind_rows(aal$detail, heat_aal$detail)
    aal$heat_yearly <- heat_aal$yearly
    assets_factors_by_rp <- dplyr::bind_rows(assets_factors_by_rp, heat_aal$factors_by_rp)
  }

  drought_aal <- NULL
  if (has_drought) {
    .report_progress(0.67, "Computing drought Average Annual Loss for every year...")
    drought_aal <- compute_global_drought_aal(
      assets = assets,
      event = drought_events[1, , drop = FALSE],
      hazard_configs = hazard_configs,
      hazards_dir = hazards_dir,
      years = unique(yearly_baseline$year)
    )
    for (msg in drought_aal$warnings) message(msg)
    drought_aal <- add_drought_aal_revenue_adjustment(drought_aal, yearly_baseline)

    aal$summary <- aal$summary |>
      dplyr::left_join(drought_aal$summary, by = "asset") |>
      dplyr::mutate(aal_drought_revenue_rate = dplyr::coalesce(.data$aal_drought_revenue_rate, 0))
    aal$detail <- dplyr::bind_rows(aal$detail, drought_aal$detail)
    aal$drought_yearly <- drought_aal$yearly
    assets_factors_by_rp <- dplyr::bind_rows(assets_factors_by_rp, drought_aal$factors_by_rp)
  }

  .report_progress(0.70, "Applying Average Annual Loss to every year...")
  yearly_shock <- compute_aal_shock_trajectories(
    yearly_baseline, aal$summary, companies,
    aal_heat_yearly = if (has_heat) heat_aal$yearly else NULL,
    aal_drought_yearly = if (has_drought) drought_aal$yearly else NULL
  )
  yearly_scenarios <- concatenate_baseline_and_shock(yearly_baseline, yearly_shock)

  .report_progress(0.82, "Computing asset financials...")
  assets_discounted_yearly <- discount_yearly_profits(yearly_scenarios, discount_rate)

  .report_progress(0.91, "Computing company financials...")
  company_yearly_trajectories <- aggregate_assets_to_company(assets_discounted_yearly)
  companies_result <- compute_companies_financials(
    companies, company_yearly_trajectories, assets_discounted_yearly, discount_rate, risk_free_rate
  )

  .report_progress(1.00, "Done.")

  out <- list(
    assets_factors = assets_factors_by_rp,
    companies = companies_result,
    assets_yearly = assets_discounted_yearly,
    companies_yearly = company_yearly_trajectories,
    aggregation_method = aggregation_method,
    aal_channels = aal
  )
  if (has_heat && length(heat_aal$warnings) > 0) {
    out$heat_warnings <- heat_aal$warnings
  }
  if (has_drought && length(drought_aal$warnings) > 0) {
    out$drought_warnings <- drought_aal$warnings
  }
  out
}
