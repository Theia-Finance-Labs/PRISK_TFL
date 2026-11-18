#' Compute per-asset AAL channels from a multi-return-period damage/cost table
#'
#' @description For each asset, reuses `evaluate_hazard_shock()` -- called once
#'   per return period, with the exact unmodified equations from the hazard
#'   config -- to compute the per-return-period loss for each of the three
#'   channels the deterministic model already distinguishes:
#'   \itemize{
#'     \item Physical damage -> profit (buildings only): a dollar amount
#'       (`damage_factor * cost_factor`), independent of any reference
#'       revenue/profit value.
#'     \item Physical damage -> revenue (agriculture only): a loss *rate*
#'       (fraction of revenue), isolated by zeroing `business_disruption` in the
#'       input before calling `evaluate_hazard_shock()` with the config's own
#'       (unmodified) agriculture revenue equation and `revenue = 1` (a unit
#'       reference, so the surviving fraction returned IS the rate).
#'     \item Business disruption -> revenue (all asset types): a loss *rate*,
#'       isolated the same way (zeroing `damage_factor`, `revenue = 1`).
#'   }
#'   Each channel's per-return-period series is then integrated into a single
#'   Average Annual Loss with `calculate_aal()` -- the same function, reused
#'   identically for all three channels (dollar and fractional alike).
#'
#'   `evaluate_hazard_shock()` is called once per return period, not once for
#'   the whole multi-RP table: its duplicate-asset guard assumes at most one
#'   row per asset per equation, which a single multi-RP call would violate.
#'
#'   No single blended "total AAL" is computed here: the physical-damage-to-
#'   profit channel is in dollars while the two revenue-rate channels are
#'   fractions, and dollars and fractions cannot be meaningfully summed into one
#'   number without a reference revenue. A true dollar total is only
#'   well-defined per year, once the revenue-rate channels are applied against
#'   that year's own baseline revenue -- see `compute_aal_shock_trajectories()`.
#' @param assets_factors_by_rp tibble: one row per (asset, return_period), with
#'   columns asset, return_period, asset_category, damage_factor, cost_factor,
#'   business_disruption (the output of `extract_flood_depth_by_rp()`)
#' @param hazard_config The hazard config list for this hazard type (e.g.
#'   `hazard_configs$Flood`), containing the `shocks.revenue`/`shocks.profit`
#'   equations
#' @return list with two elements:
#'   - `summary`: tibble with columns asset, aal_pd_profit ($, buildings, 0
#'     otherwise), aal_pd_revenue_rate (fraction, agriculture, 0 otherwise),
#'     aal_bd_revenue_rate (fraction, all asset types) -- the integrated AAL
#'     per asset per channel, used to build the adverse trajectory.
#'   - `detail`: long-format tibble (asset, return_period, channel, loss, aep)
#'     with the exact per-return-period loss values that fed each channel's
#'     `calculate_aal()` integration -- the audit trail for the "loss per
#'     return period" diagnostic table, using the values actually produced by
#'     the config's own equations rather than a re-derived approximation.
#' @noRd
compute_aal_channels <- function(assets_factors_by_rp, hazard_config) {
  empty_summary <- tibble::tibble(
    asset = character(),
    aal_pd_profit = numeric(),
    aal_pd_revenue_rate = numeric(),
    aal_bd_revenue_rate = numeric()
  )
  empty_detail <- tibble::tibble(
    asset = character(), return_period = numeric(), channel = character(),
    loss = numeric(), aep = numeric()
  )
  empty_result <- list(summary = empty_summary, detail = empty_detail)

  if (is.null(assets_factors_by_rp) || nrow(assets_factors_by_rp) == 0) {
    return(empty_result)
  }

  required_cols <- c(
    "asset", "return_period", "damage_factor", "cost_factor",
    "business_disruption", "asset_category"
  )
  missing_cols <- setdiff(required_cols, names(assets_factors_by_rp))
  if (length(missing_cols) > 0) {
    stop(
      "compute_aal_channels: assets_factors_by_rp is missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }

  base <- assets_factors_by_rp |>
    dplyr::mutate(
      damage_factor = as.numeric(.data$damage_factor),
      cost_factor = as.numeric(.data$cost_factor),
      business_disruption = as.numeric(.data$business_disruption)
    )

  return_periods <- sort(unique(base$return_period))
  channel_losses <- vector("list", length(return_periods) * 3L)
  idx <- 0L

  for (rp in return_periods) {
    rp_rows <- base |> dplyr::filter(.data$return_period == rp)
    if (nrow(rp_rows) == 0) next

    # Physical damage -> profit (buildings). Any placeholder profit value works:
    # the returned delta is damage_factor * cost_factor, algebraically
    # independent of the input profit (confirmed in evaluate_hazard_shock()'s
    # profit branch: baseline_profit - new_profit cancels the input value out).
    pd_profit_shock <- evaluate_hazard_shock(
      dplyr::mutate(rp_rows, profit = 0), hazard_config, "profit", combine = "error"
    )
    if (nrow(pd_profit_shock) > 0) {
      idx <- idx + 1L
      channel_losses[[idx]] <- tibble::tibble(
        asset = pd_profit_shock$asset, return_period = rp,
        channel = "aal_pd_profit", loss = pd_profit_shock$shock_value
      )
    }

    # Business disruption -> revenue (all asset types). Zero damage_factor so
    # agriculture's combined equation (revenue * (1-damage_factor) *
    # (1-business_disruption/365)) reduces to the pure business-disruption
    # term; revenue = 1 (unit reference) so 1 - shock_value IS the loss rate.
    bd_revenue_shock <- evaluate_hazard_shock(
      dplyr::mutate(rp_rows, revenue = 1, damage_factor = 0), hazard_config, "revenue", combine = "error"
    )
    if (nrow(bd_revenue_shock) > 0) {
      idx <- idx + 1L
      channel_losses[[idx]] <- tibble::tibble(
        asset = bd_revenue_shock$asset, return_period = rp,
        channel = "aal_bd_revenue_rate", loss = 1 - bd_revenue_shock$shock_value
      )
    }

    # Physical damage -> revenue (agriculture only). Zero business_disruption
    # so the combined equation reduces to the pure damage-factor term. For
    # non-agriculture assets the revenue equation has no damage_factor term at
    # all, so this correctly evaluates to a loss of 0 for them.
    pd_revenue_shock <- evaluate_hazard_shock(
      dplyr::mutate(rp_rows, revenue = 1, business_disruption = 0), hazard_config, "revenue", combine = "error"
    )
    if (nrow(pd_revenue_shock) > 0) {
      idx <- idx + 1L
      channel_losses[[idx]] <- tibble::tibble(
        asset = pd_revenue_shock$asset, return_period = rp,
        channel = "aal_pd_revenue_rate", loss = 1 - pd_revenue_shock$shock_value
      )
    }
  }

  channel_losses <- channel_losses[!vapply(channel_losses, is.null, logical(1))]
  if (length(channel_losses) == 0) {
    return(empty_result)
  }

  all_losses <- dplyr::bind_rows(channel_losses) |>
    dplyr::mutate(aep = 1 / .data$return_period)

  # An asset can legitimately have no usable (return_period, loss) pairs for a
  # given channel -- e.g. a precomputed-lookup asset whose region has no
  # coverage in the hazard data for some return periods, leaving damage_factor/
  # business_disruption NA there. calculate_aal() rightly errors on genuinely
  # empty input (a real bug elsewhere), but that "no coverage" case is expected
  # data-completeness noise here, not a bug -- treat it as zero AAL contribution
  # for that asset+channel rather than failing the whole portfolio's run.
  safe_calculate_aal <- function(rp, loss) {
    if (!any(!is.na(rp) & !is.na(loss))) {
      return(0)
    }
    calculate_aal(rp, loss)
  }

  aal_by_channel <- all_losses |>
    dplyr::group_by(.data$asset, .data$channel) |>
    dplyr::summarize(aal_value = safe_calculate_aal(.data$return_period, .data$loss), .groups = "drop") |>
    tidyr::pivot_wider(names_from = "channel", values_from = "aal_value", values_fill = 0)

  for (col in c("aal_pd_profit", "aal_bd_revenue_rate", "aal_pd_revenue_rate")) {
    if (!col %in% names(aal_by_channel)) aal_by_channel[[col]] <- 0
  }

  list(
    summary = aal_by_channel |>
      dplyr::select("asset", "aal_pd_profit", "aal_pd_revenue_rate", "aal_bd_revenue_rate"),
    detail = all_losses |>
      dplyr::select("asset", "return_period", "channel", "loss", "aep") |>
      dplyr::arrange(.data$asset, .data$channel, .data$return_period)
  )
}
