#' Apply the Average Annual Loss (AAL) as a constant deduction to every year of
#' the baseline trajectory
#'
#' @description The probabilistic-mode counterpart to `compute_shock_trajectories()`,
#'   mirroring its exact 3-step sequence (revenue shock -> recompute profit from
#'   the shocked revenue -> additional profit deduction), but applying the AAL to
#'   *every* year of the horizon instead of a single `event_year`:
#'   \enumerate{
#'     \item `revenue(t) = baseline_revenue(t) * (1 - aal_pd_revenue_rate - aal_bd_revenue_rate)`,
#'       for every year `t`. The two revenue-rate AAL channels (physical damage
#'       for agriculture, business disruption for every asset type) are combined
#'       additively -- they were integrated independently across return periods
#'       (see `compute_aal_channels()`), so summing their rates is the standard
#'       way to combine two independently-annualized expected losses; this
#'       differs from the deterministic single-event model, which combines them
#'       multiplicatively because that equation describes one point-in-time event.
#'     \item `profit(t) = revenue(t) * net_profit_margin` -- reuses
#'       `compute_profits_from_revenue()` completely unchanged.
#'     \item `profit(t) = profit(t) - aal_pd_profit` (a flat dollar amount,
#'       buildings only, 0 otherwise), for every year `t` -- mirrors
#'       `apply_acute_profit_shock()`'s "additional deduction on top of step 2"
#'       pattern, just applied to every row instead of one `event_year` row.
#'   }
#' @param yearly_baseline_profits tibble with columns: asset, company, year, revenue, profit
#'   (the output of `compute_baseline_trajectories()`, unmodified)
#' @param aal_channels tibble with columns: asset, aal_pd_profit, aal_pd_revenue_rate,
#'   aal_bd_revenue_rate (the output of `compute_aal_channels()`)
#' @param companies tibble with columns: company, net_profit_margin
#' @param aal_heat_yearly Optional tibble(asset, year, aal_lp_revenue_rate) from
#'   `compute_global_heat_aal()`. Heat is the one AAL channel that varies by year
#'   (its maps are blended to each year's climate), so it is joined by asset AND
#'   year and subtracted additively in the revenue step like the other revenue-rate
#'   channels. NULL (the default) leaves the flood-only behavior unchanged.
#' @param aal_drought_yearly Optional tibble(asset, year, aal_drought_revenue_rate) from
#'   `compute_global_drought_aal()`; year-varying like heat and subtracted the same way.
#' @return tibble with columns: asset, company, year, revenue, profit -- the same
#'   shape `compute_shock_trajectories()` returns, so `concatenate_baseline_and_shock()`
#'   accepts it unmodified
#' @noRd
compute_aal_shock_trajectories <- function(yearly_baseline_profits, aal_channels, companies, aal_heat_yearly = NULL,
                                           aal_drought_yearly = NULL) {
  if (is.null(yearly_baseline_profits) || nrow(yearly_baseline_profits) == 0) {
    return(yearly_baseline_profits)
  }

  base_cols <- c("asset", "company", "year", "revenue")
  missing_base_cols <- setdiff(base_cols, names(yearly_baseline_profits))
  if (length(missing_base_cols) > 0) {
    stop(
      "compute_aal_shock_trajectories: yearly_baseline_profits is missing required column(s): ",
      paste(missing_base_cols, collapse = ", ")
    )
  }

  if (is.null(aal_channels) || nrow(aal_channels) == 0) {
    # No AAL data at all -- adverse scenario equals baseline. Mirrors
    # compute_shock_trajectories()'s own no-events no-op path.
    return(yearly_baseline_profits |> dplyr::select("asset", "company", "year", "revenue", "profit"))
  }

  # STEP 1: apply the revenue-rate AAL channels to every year's baseline revenue.
  current_trajectories <- yearly_baseline_profits |>
    dplyr::select("asset", "company", "year", "revenue") |>
    dplyr::left_join(
      aal_channels |> dplyr::select(-dplyr::any_of(c(
        "aal_lp_revenue_rate", "avg_annual_revenue_adjustment",
        "aal_drought_revenue_rate", "avg_annual_drought_revenue_adjustment"
      ))),
      by = "asset"
    ) |>
    dplyr::mutate(
      aal_pd_profit = dplyr::coalesce(.data$aal_pd_profit, 0),
      aal_pd_revenue_rate = dplyr::coalesce(.data$aal_pd_revenue_rate, 0),
      aal_bd_revenue_rate = dplyr::coalesce(.data$aal_bd_revenue_rate, 0)
    )

  if (!is.null(aal_heat_yearly) && nrow(aal_heat_yearly) > 0) {
    current_trajectories <- current_trajectories |>
      dplyr::left_join(
        aal_heat_yearly |> dplyr::select("asset", "year", "aal_lp_revenue_rate"),
        by = c("asset", "year")
      ) |>
      dplyr::mutate(aal_lp_revenue_rate = dplyr::coalesce(.data$aal_lp_revenue_rate, 0))
  } else {
    current_trajectories$aal_lp_revenue_rate <- 0
  }

  if (!is.null(aal_drought_yearly) && nrow(aal_drought_yearly) > 0) {
    current_trajectories <- current_trajectories |>
      dplyr::left_join(
        aal_drought_yearly |> dplyr::select("asset", "year", "aal_drought_revenue_rate"),
        by = c("asset", "year")
      ) |>
      dplyr::mutate(aal_drought_revenue_rate = dplyr::coalesce(.data$aal_drought_revenue_rate, 0))
  } else {
    current_trajectories$aal_drought_revenue_rate <- 0
  }

  current_trajectories <- current_trajectories |>
    dplyr::mutate(
      revenue = pmax(
        0,
        .data$revenue * (1 - .data$aal_pd_revenue_rate - .data$aal_bd_revenue_rate -
          .data$aal_lp_revenue_rate - .data$aal_drought_revenue_rate)
      )
    )

  # STEP 2: profit(t) = revenue(t) * net_profit_margin -- reused unchanged.
  # aal_pd_profit is carried through so step 3 can still deduct it afterward.
  current_trajectories <- compute_profits_from_revenue(
    current_trajectories |> dplyr::select("asset", "company", "year", "revenue", "aal_pd_profit"),
    companies
  )

  # STEP 3: additional flat-dollar profit deduction (buildings only), every year.
  current_trajectories |>
    dplyr::mutate(profit = .data$profit - .data$aal_pd_profit) |>
    dplyr::select("asset", "company", "year", "revenue", "profit")
}
