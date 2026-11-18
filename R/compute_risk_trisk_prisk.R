#' Compute TRISK-PRISK Integrated Risk Analysis
#'
#' @title TRISK-PRISK orchestrator -- parallel to compute_risk()/compute_risk_probabilistic(), never calls either
#' @description Top-level orchestrator for the TRISK-PRISK integration mode. Never
#'   calls or modifies `compute_risk()`, `compute_risk_probabilistic()`, or any of
#'   the deterministic/probabilistic single-hazard shock functions -- those paths
#'   are completely unaffected by this function's existence.
#'
#'   Produces four company-level scenario pathways (`trisk_baseline`,
#'   `trisk_shock`, `trisk_prisk_baseline`, `trisk_prisk_shock`) by chaining,
#'   unmodified: `compute_trisk_aal_by_year()`, `apply_trisk_prisk_adjustment()`,
#'   `discount_yearly_profits()`, `aggregate_assets_to_company()`,
#'   `compute_company_npv()`, `compute_pd_merton()`, `compute_expected_loss()`
#'   (and, when `include_terminal_value` is TRUE, `compute_terminal_value()`).
#'   `gather_and_pivot_results()` is deliberately not reused here -- it hardcodes
#'   exactly two scenario labels (`"baseline"`/`"shock"`), which doesn't fit four
#'   -- `pivot_trisk_prisk_results()` replaces it for this mode only.
#' @param trisk_joined tibble from `join_trisk_to_prisk_assets()`
#' @param companies tibble from `read_companies()` (PRISK financials/portfolio;
#'   must have `company`, `net_profit_margin`, `debt`, `volatility`, `term`,
#'   `lgd`, `loan_size`)
#' @param hazards,hazards_inventory,hazard_configs,hazards_dir,precomputed_hazards
#'   Loaded hazard data/config, same as `compute_risk()`/`compute_risk_probabilistic()`
#' @param growth_rate,discount_rate Numeric, from the app's existing Financial
#'   Parameters settings (not new inputs for this mode)
#' @param risk_free_rate Numeric, for the Merton PD calculation (default 0.02)
#' @param include_terminal_value Logical, the terminal-value toggle (default TRUE)
#' @param years Integer vector of pathway years (default 2026:2050)
#' @param anchor_year Numeric, present-day interpolation/discounting anchor (default 2026)
#' @param endpoint_year Numeric, the year the RCP8.5/RCP2.6 endpoint AAL is anchored to (default 2075)
#' @param aggregation_method Character, passed through to hazard extraction (default "mean")
#' @param on_progress Optional function(value, message) called at each pipeline step
#' @return list with:
#'   - `companies`: wide tibble, one row per company, with `npv_*`, `pd_*`,
#'     `expected_loss_*` columns for each of the four scenarios (the "eight" [sic,
#'     actually twelve: 3 metrics x 4 scenarios] final figures per company)
#'   - `companies_long`: the same results in long format (company, scenario, npv,
#'     merton_pd, expected_loss, ...)
#'   - `company_yearly`: company-level yearly trajectories for all four scenarios
#'     (feeds the Profit Pathways comparison charts)
#'   - `aal`: the full `compute_trisk_aal_by_year()` output (by_year, endpoints, detail)
#'   - `terminal_value`: `compute_terminal_value()`'s output, or NULL if the toggle was off
#' @export
compute_risk_trisk_prisk <- function(
  trisk_joined,
  companies,
  hazards,
  hazards_inventory,
  hazard_configs,
  hazards_dir,
  precomputed_hazards,
  growth_rate = 0.02,
  discount_rate = 0.05,
  risk_free_rate = 0.02,
  include_terminal_value = TRUE,
  years = 2026:2050,
  anchor_year = 2026,
  endpoint_year = 2075,
  aggregation_method = "mean",
  on_progress = NULL
) {
  .report_progress <- function(value, msg) {
    message(sprintf("[compute_risk_trisk_prisk] %3d%%  %s", round(value * 100), msg))
    if (!is.null(on_progress)) {
      tryCatch(on_progress(value, msg), error = function(e) message("[progress callback error] ", conditionMessage(e)))
    }
  }

  if (!is.data.frame(trisk_joined) || nrow(trisk_joined) == 0) {
    stop("compute_risk_trisk_prisk: trisk_joined must be a non-empty data.frame")
  }
  if (!is.data.frame(companies) || nrow(companies) == 0) {
    stop("compute_risk_trisk_prisk: companies must be a non-empty data.frame")
  }

  hazards_inventory <- drop_heat_ui_inventory_columns(hazards_inventory)

  assets_unique <- trisk_joined |> dplyr::distinct(.data$asset, .keep_all = TRUE)

  .report_progress(0.10, "Computing interpolated AAL pathways...")
  aal <- compute_trisk_aal_by_year(
    assets = assets_unique,
    hazards = hazards,
    hazards_inventory = hazards_inventory,
    hazard_configs = hazard_configs,
    hazards_dir = hazards_dir,
    precomputed_hazards = precomputed_hazards,
    years = years,
    anchor_year = anchor_year,
    endpoint_year = endpoint_year,
    aggregation_method = aggregation_method
  )

  .report_progress(0.40, "Applying physical-risk adjustment to TRISK pathways...")
  adjusted <- apply_trisk_prisk_adjustment(trisk_joined, aal$by_year, companies)

  .report_progress(0.55, "Discounting and aggregating to company level...")
  discounted <- discount_yearly_profits(adjusted, discount_rate = discount_rate, base_year = anchor_year)
  company_yearly <- aggregate_assets_to_company(discounted)

  .report_progress(0.65, "Computing NPV...")
  companies_npv <- compute_company_npv(company_yearly)

  terminal_value <- NULL
  if (isTRUE(include_terminal_value)) {
    .report_progress(0.75, "Computing terminal value...")
    terminal_value <- compute_terminal_value(
      trisk_joined = trisk_joined,
      aal_endpoints = aal$endpoints,
      companies = companies,
      growth_rate = growth_rate,
      discount_rate = discount_rate,
      terminal_year = max(years),
      anchor_year = anchor_year
    )
    companies_npv <- companies_npv |>
      dplyr::left_join(
        terminal_value |> dplyr::select("company", "scenario", "tv_present_value"),
        by = c("company", "scenario")
      ) |>
      dplyr::mutate(npv = .data$npv + dplyr::coalesce(.data$tv_present_value, 0)) |>
      dplyr::select(-"tv_present_value")
  }

  .report_progress(0.85, "Computing PD and Expected Loss...")
  companies_with_financials <- dplyr::left_join(companies, companies_npv, by = "company")
  companies_pd <- compute_pd_merton(companies_with_financials, risk_free_rate)
  companies_el <- compute_expected_loss(companies_pd)

  .report_progress(0.95, "Formatting results...")
  companies_pivoted <- pivot_trisk_prisk_results(companies_el)

  .report_progress(1.00, "Done.")

  list(
    companies = companies_pivoted,
    companies_long = companies_el,
    company_yearly = company_yearly,
    aal = aal,
    terminal_value = terminal_value,
    include_terminal_value = isTRUE(include_terminal_value)
  )
}

#' Pivot TRISK-PRISK company results to one row per company
#'
#' @title Pivot four-scenario TRISK-PRISK results to wide format
#' @description Replaces `gather_and_pivot_results()` for this mode only, since
#'   that function hardcodes exactly two scenario labels
#'   (`"baseline"`/`"shock"`), which doesn't fit the four scenarios here. The
#'   underlying NPV/PD/EL computation functions this wraps are otherwise fully
#'   reused, unmodified.
#' @param companies_el tibble with columns `company`, `scenario`, `npv`,
#'   `merton_pd`, `expected_loss` -- one row per company per scenario (the
#'   output of `compute_expected_loss()`)
#' @return wide tibble, one row per company, with `npv_<scenario>`,
#'   `pd_<scenario>`, `expected_loss_<scenario>` columns for each of the four
#'   scenarios
#' @noRd
pivot_trisk_prisk_results <- function(companies_el) {
  companies_el |>
    dplyr::mutate(scenario = as.character(.data$scenario)) |>
    dplyr::select("company", "scenario", npv = "npv", pd = "merton_pd", expected_loss = "expected_loss") |>
    tidyr::pivot_wider(
      names_from = "scenario",
      values_from = c("npv", "pd", "expected_loss"),
      names_glue = "{.value}_{scenario}"
    ) |>
    dplyr::arrange(.data$company)
}
