#' Apply the physical-risk (AAL) adjustment to TRISK asset-level profit pathways
#'
#' @title TRISK-PRISK asset-level profit pathway adjustment
#' @description For the TRISK-PRISK integration mode, deducts the interpolated AAL
#'   (from `compute_trisk_aal_by_year()`) from TRISK's externally-supplied
#'   asset-level profit pathways, for every year, following exactly the same
#'   asset-category channel split as `compute_aal_shock_trajectories()`:
#'   \enumerate{
#'     \item If TRISK's profit for an asset/year/pathway is \code{NA} or `<= 0`,
#'       skip all adjustment -- the output equals that value unchanged (subject
#'       to the floor below). No revenue is reverse-engineered and no AAL is
#'       deducted for that cell.
#'     \item Otherwise, `revenue(t) = profit(t) / net_profit_margin` (NPM always
#'       from the PRISK company/financials input, never from the TRISK file).
#'     \item Non-agriculture (industrial/commercial buildings):
#'       `revenue_adjusted(t) = revenue(t) * (1 - aal_bd_revenue_rate(t))`, then
#'       `profit_adjusted(t) = revenue_adjusted(t) * NPM`, then
#'       `profit_final(t) = profit_adjusted(t) - aal_pd_profit(t)` (a flat dollar
#'       deduction, mirroring `apply_acute_profit_shock()`'s "additional
#'       deduction on top" pattern).
#'     \item Agriculture: `revenue_adjusted(t) = revenue(t) * (1 -
#'       aal_bd_revenue_rate(t) - aal_pd_revenue_rate(t))`, then
#'       `profit_final(t) = revenue_adjusted(t) * NPM` -- physical damage is
#'       folded into the revenue-rate deduction, no separate profit-side
#'       deduction (mirrors the deterministic model's agriculture channel split).
#'   }
#'   Every profit pathway (all four scenarios) is floored at 0: negative profit
#'   is never returned, whether it comes from TRISK's own (unadjusted) input or
#'   from an AAL deduction that would otherwise push profit below zero.
#'   Produces four scenario pathways per asset/year: the two original,
#'   *unadjusted* TRISK pathways (`trisk_baseline`, `trisk_shock` -- passed
#'   through verbatim, floor applied) and the two AAL-adjusted pathways
#'   (`trisk_prisk_baseline`, `trisk_prisk_shock`).
#' @param trisk_joined tibble from `join_trisk_to_prisk_assets()`: one row per
#'   asset-year, with `asset`, `company`, `asset_category`, `year`,
#'   `net_profits_baseline_scenario`, `net_profits_shock_scenario`
#' @param aal_by_year tibble from `compute_trisk_aal_by_year()$by_year`: `asset`,
#'   `year`, `pathway` ("baseline"/"shock"), `aal_pd_profit`,
#'   `aal_pd_revenue_rate`, `aal_bd_revenue_rate`
#' @param companies tibble with columns `company`, `net_profit_margin` (PRISK
#'   financials input)
#' @return long tibble: `asset`, `company`, `year`, `scenario` (one of
#'   `trisk_baseline`, `trisk_shock`, `trisk_prisk_baseline`,
#'   `trisk_prisk_shock`), `revenue`, `profit`. `revenue` is only ever the
#'   reverse-engineered value used internally to reach `profit` -- it is not a
#'   meaningful output on its own and is not exposed further downstream.
#' @export
apply_trisk_prisk_adjustment <- function(trisk_joined, aal_by_year, companies) {
  required_cols <- c(
    "asset", "company", "asset_category", "year",
    "net_profits_baseline_scenario", "net_profits_shock_scenario"
  )
  missing_cols <- setdiff(required_cols, names(trisk_joined))
  if (length(missing_cols) > 0) {
    stop(
      "apply_trisk_prisk_adjustment: trisk_joined is missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }

  npm_lookup <- companies |>
    dplyr::select("company", "net_profit_margin") |>
    dplyr::distinct(.data$company, .keep_all = TRUE)

  trisk_long <- trisk_joined |>
    dplyr::select(
      "asset", "company", "asset_category", "year",
      trisk_baseline = "net_profits_baseline_scenario",
      trisk_shock = "net_profits_shock_scenario"
    ) |>
    tidyr::pivot_longer(
      cols = c("trisk_baseline", "trisk_shock"),
      names_to = "pathway_label",
      values_to = "trisk_profit"
    ) |>
    dplyr::mutate(pathway = sub("^trisk_", "", .data$pathway_label))

  missing_npm <- setdiff(unique(trisk_long$company), npm_lookup$company)
  if (length(missing_npm) > 0) {
    stop(
      "apply_trisk_prisk_adjustment: missing net_profit_margin for company/companies: ",
      paste(missing_npm, collapse = ", ")
    )
  }

  # Unadjusted TRISK-only pathways: passed through verbatim (no AAL involved),
  # except profit is floored at 0 -- negative profit pathways are not allowed.
  trisk_only <- trisk_long |>
    dplyr::select("asset", "company", "year", scenario = "pathway_label", profit = "trisk_profit") |>
    dplyr::mutate(
      revenue = NA_real_,
      profit = pmax(0, .data$profit)
    ) |>
    dplyr::select("asset", "company", "year", "scenario", "revenue", "profit")

  # TRISK+PRISK adjusted pathways.
  combined <- trisk_long |>
    dplyr::left_join(aal_by_year, by = c("asset", "year", "pathway")) |>
    dplyr::mutate(
      aal_pd_profit = dplyr::coalesce(.data$aal_pd_profit, 0),
      aal_pd_revenue_rate = dplyr::coalesce(.data$aal_pd_revenue_rate, 0),
      aal_bd_revenue_rate = dplyr::coalesce(.data$aal_bd_revenue_rate, 0)
    ) |>
    dplyr::left_join(npm_lookup, by = "company")

  skip <- is.na(combined$trisk_profit) | combined$trisk_profit <= 0
  is_agriculture <- combined$asset_category == "agriculture"

  revenue <- combined$trisk_profit / combined$net_profit_margin

  revenue_adjusted <- dplyr::if_else(
    is_agriculture,
    revenue * (1 - combined$aal_bd_revenue_rate - combined$aal_pd_revenue_rate),
    revenue * (1 - combined$aal_bd_revenue_rate)
  )
  profit_adjusted <- revenue_adjusted * combined$net_profit_margin
  profit_final <- dplyr::if_else(
    is_agriculture,
    profit_adjusted,
    profit_adjusted - combined$aal_pd_profit
  )

  trisk_prisk <- combined |>
    dplyr::mutate(
      revenue = dplyr::if_else(skip, NA_real_, revenue_adjusted),
      profit = pmax(0, dplyr::if_else(skip, .data$trisk_profit, profit_final)),
      scenario = paste0("trisk_prisk_", .data$pathway)
    ) |>
    dplyr::select("asset", "company", "year", "scenario", "revenue", "profit")

  dplyr::bind_rows(trisk_only, trisk_prisk)
}
