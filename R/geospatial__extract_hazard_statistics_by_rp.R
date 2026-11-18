#' Build a synthetic "one row per return period" events table (internal)
#'
#' @description For probabilistic (AAL) mode, every available return period for a
#'   given hazard type + scenario is needed at once (not a single event), so this
#'   builds a synthetic events-shaped tibble -- one row per return period, each
#'   with its own distinct event_id -- that can be fed through the same generic,
#'   per-event-row functions already used by the deterministic path
#'   (filter_hazards_by_events(), extract_hazard_statistics(),
#'   create_event_hazard_mapping()). None of those functions assume a single
#'   return period across all rows, so no new extraction logic is needed -- only
#'   this small events-table construction step.
#' @param hazards_inventory tibble from load_hazards_and_inventory()$inventory
#' @param hazard_type Character, e.g. "Flood"
#' @param scenario_name Character, e.g. "present", "rcp26", "rcp85"
#' @return tibble with columns: event_id, hazard_type, scenario_name,
#'   return_period, event_year (NA -- not meaningful for AAL rows)
#' @noRd
build_probabilistic_events_table <- function(hazards_inventory, hazard_type, scenario_name) {
  if (is.null(hazards_inventory) || nrow(hazards_inventory) == 0) {
    stop("build_probabilistic_events_table: hazards_inventory is empty")
  }

  return_periods <- hazards_inventory |>
    dplyr::filter(.data$hazard_type == !!hazard_type, .data$scenario_name == !!scenario_name) |>
    dplyr::distinct(.data$return_period) |>
    dplyr::filter(!is.na(.data$return_period)) |>
    dplyr::pull(.data$return_period) |>
    sort()

  if (length(return_periods) == 0) {
    stop(
      "build_probabilistic_events_table: no return periods found for hazard_type='",
      hazard_type, "', scenario_name='", scenario_name, "'"
    )
  }

  tibble::tibble(
    event_id = paste0("aal_rp_", seq_along(return_periods)),
    hazard_type = hazard_type,
    scenario_name = scenario_name,
    return_period = return_periods,
    event_year = NA_integer_
  )
}

#' Extract hazard intensity and join damage/cost factors for every available
#' return period of a scenario (internal)
#'
#' @description Reuses the exact same, generic per-event-row functions the
#'   deterministic pipeline uses (filter_hazards_by_events(),
#'   extract_hazard_statistics(), create_event_hazard_mapping(),
#'   join_damage_cost_factors()) with a synthetic multi-row "one row per return
#'   period" events table (build_probabilistic_events_table()), so the
#'   extraction/damage-join logic itself is never duplicated -- only the RP-events
#'   table construction and the final indicator_key -> event_id attachment are
#'   new. Deliberately skips the multi-indicator column-reconciliation logic
#'   compute_risk() needs for hazards like Fire, since it doesn't apply to
#'   single-indicator Flood: `extract_hazard_statistics()`'s output already
#'   carries its own scenario_name/return_period/hazard_name (from each raster's
#'   own metadata), so only event_id needs to be joined in here.
#' @param assets tibble of asset data (from read_assets(), already filtered to
#'   companies present in the portfolio)
#' @param hazards Named list of SpatRaster objects (from load_hazards_and_inventory())
#' @param hazards_inventory tibble from load_hazards_and_inventory()$inventory
#' @param hazard_configs Named list from load_hazards_and_inventory()$configs
#' @param hazards_dir Character path to the hazards config directory (for locating
#'   the shared mapping tables, e.g. damage_and_cost_factors.csv)
#' @param precomputed_hazards tibble from read_precomputed_hazards() (used only for
#'   assets without lat/long coordinates)
#' @param hazard_type Character, e.g. "Flood"
#' @param scenario_name Character, e.g. "present", "rcp26", "rcp85"
#' @param aggregation_method Character, passed through to extract_hazard_statistics()
#' @return tibble: one row per (asset, return_period), with damage_factor,
#'   cost_factor, business_disruption, asset_category, and return_period columns
#' @noRd
extract_flood_depth_by_rp <- function(
  assets,
  hazards,
  hazards_inventory,
  hazard_configs,
  hazards_dir,
  precomputed_hazards,
  hazard_type,
  scenario_name,
  aggregation_method = "mean"
) {
  rp_events <- build_probabilistic_events_table(hazards_inventory, hazard_type, scenario_name)

  hazards_for_rp <- filter_hazards_by_events(hazards, rp_events, hazards_inventory, hazard_configs)
  if (length(hazards_for_rp) == 0) {
    stop(
      "extract_flood_depth_by_rp: no hazard rasters available for hazard_type='",
      hazard_type, "', scenario_name='", scenario_name, "'"
    )
  }

  filtered_inventory <- hazards_inventory |>
    dplyr::filter(.data$indicator_key %in% names(hazards_for_rp))

  assets_long <- extract_hazard_statistics(
    assets_df = assets,
    hazards = hazards_for_rp,
    hazards_inventory = filtered_inventory,
    precomputed_hazards = precomputed_hazards,
    hazard_configs = hazard_configs,
    aggregation_method = aggregation_method
  )

  events_expanded <- create_event_hazard_mapping(rp_events, hazards_inventory, hazard_configs)
  event_id_lookup <- events_expanded |>
    dplyr::distinct(.data$indicator_key, .data$event_id)

  assets_with_events <- assets_long |>
    dplyr::inner_join(event_id_lookup, by = "indicator_key")

  if (nrow(assets_with_events) == 0) {
    stop(
      "extract_flood_depth_by_rp: hazard extraction produced no rows for hazard_type='",
      hazard_type, "', scenario_name='", scenario_name, "'"
    )
  }

  join_damage_cost_factors(assets_with_events, hazard_configs, hazards_dir)
}
