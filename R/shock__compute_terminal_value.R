#' Compute the terminal value for TRISK-PRISK company pathways
#'
#' @title TRISK-PRISK terminal value (growing perpetuity)
#' @description Computes a Gordon-growth terminal value for each of the four
#'   TRISK-PRISK scenario pathways, to optionally add to NPV:
#'   \enumerate{
#'     \item Take every asset's `terminal_year` TRISK profit (before any AAL
#'       adjustment).
#'     \item Reuses `apply_trisk_prisk_adjustment()` completely unchanged to
#'       compute a "terminal profit" per asset -- but fed a synthetic AAL table
#'       built from the full, non-interpolated endpoint AAL values
#'       (`aal_endpoints$rcp85` for the baseline pathway, `aal_endpoints$rcp26`
#'       for the shock pathway) rather than `terminal_year`'s interpolated rate.
#'       This also naturally produces the `trisk_baseline`/`trisk_shock`
#'       terminal profits (TRISK-only, unadjusted, since
#'       `apply_trisk_prisk_adjustment()` always returns all four scenarios) --
#'       used as-is for those two pathways' terminal values, since there is no
#'       physical-risk component to apply to a TRISK-only pathway.
#'     \item Aggregates the resulting one-year asset-level terminal profits to
#'       company level, reusing `discount_yearly_profits()` +
#'       `aggregate_assets_to_company()` unchanged (the one-year discount factor
#'       from this step is not used -- only `total_profit`, since the perpetuity
#'       discounting below is applied once, separately, to the terminal value).
#'     \item `TV_nominal = terminal_profit * (1 + growth_rate) / (discount_rate - growth_rate)`.
#'     \item `TV_present = TV_nominal / (1 + discount_rate)^(terminal_year - anchor_year)`.
#'   }
#' @param trisk_joined tibble from `join_trisk_to_prisk_assets()`
#' @param aal_endpoints list(present, rcp26, rcp85) from
#'   `compute_trisk_aal_by_year()$endpoints` -- only `rcp85`/`rcp26` are used
#' @param companies tibble with columns `company`, `net_profit_margin`
#' @param growth_rate Numeric, the perpetuity growth rate (must be `< discount_rate`)
#' @param discount_rate Numeric, the discount rate
#' @param terminal_year Numeric, the last modeled pathway year (default 2050)
#' @param anchor_year Numeric, the present-value base year (default 2026)
#' @return tibble: `company`, `scenario`, `tv_nominal`, `tv_present_value`
#' @export
compute_terminal_value <- function(
  trisk_joined,
  aal_endpoints,
  companies,
  growth_rate,
  discount_rate,
  terminal_year = 2050,
  anchor_year = 2026
) {
  if (!isTRUE(growth_rate < discount_rate)) {
    stop(
      "compute_terminal_value: growth_rate must be less than discount_rate for a ",
      "convergent growing perpetuity (growth_rate=", growth_rate, ", discount_rate=", discount_rate, ")"
    )
  }

  terminal_trisk <- trisk_joined |> dplyr::filter(.data$year == terminal_year)
  if (nrow(terminal_trisk) == 0) {
    stop("compute_terminal_value: no TRISK rows found for terminal_year = ", terminal_year)
  }

  channel_cols <- c("aal_pd_profit", "aal_pd_revenue_rate", "aal_bd_revenue_rate")
  missing_endpoints <- setdiff(c("rcp85", "rcp26"), names(aal_endpoints))
  if (length(missing_endpoints) > 0) {
    stop("compute_terminal_value: aal_endpoints is missing: ", paste(missing_endpoints, collapse = ", "))
  }

  # The full, non-interpolated endpoint AAL -- not terminal_year's interpolated
  # rate -- used at the (synthetic) terminal_year so apply_trisk_prisk_adjustment()
  # can be reused unchanged.
  endpoint_aal_by_year <- dplyr::bind_rows(
    aal_endpoints$rcp85 |> dplyr::mutate(year = terminal_year, pathway = "baseline"),
    aal_endpoints$rcp26 |> dplyr::mutate(year = terminal_year, pathway = "shock")
  ) |>
    dplyr::select("asset", "year", "pathway", dplyr::all_of(channel_cols))

  terminal_profit_asset <- apply_trisk_prisk_adjustment(terminal_trisk, endpoint_aal_by_year, companies)

  terminal_profit_company <- terminal_profit_asset |>
    discount_yearly_profits(discount_rate = discount_rate, base_year = terminal_year) |>
    aggregate_assets_to_company()

  terminal_profit_company |>
    dplyr::mutate(
      tv_nominal = .data$total_profit * (1 + growth_rate) / (discount_rate - growth_rate),
      tv_present_value = .data$tv_nominal / (1 + discount_rate)^(terminal_year - anchor_year)
    ) |>
    dplyr::select("company", "scenario", "tv_nominal", "tv_present_value")
}
