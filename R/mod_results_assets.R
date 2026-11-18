#' results_assets UI Function
#'
#' @description Module to display asset-level results table with exposures
#' @param id Internal parameter for shiny
#' @export
mod_results_assets_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "results-container",
      shiny::h3("Asset Exposures", class = "results-title"),
      shiny::p(
        "Expand an event to review the associated assets and impact metrics.",
        class = "text-muted",
        style = "margin-bottom: 1.5rem;"
      ),
      shiny::div(
        class = "results-downloads",
        shiny::downloadButton(
          ns("download_assets_csv"),
          "Download Assets (CSV)",
          class = "btn btn-info"
        ),
        shiny::downloadButton(
          ns("download_assets_excel"),
          "Download Assets (Excel)",
          class = "btn btn-info"
        )
      ),
      shiny::uiOutput(ns("hazard_tables"))
    )
  )
}

#' Build the probabilistic-mode Asset Exposures table (internal)
#'
#' @noRd
# Heat columns kept in the results object (backend) but not shown in the Asset Analysis
# tables or the CSV/Excel downloads: intermediate or matching diagnostics rather than results.
HEAT_BACKEND_ONLY_COLS <- c(
  "s", "baseline_revenue", "revenue_adjustment", "nace_level", "nace_match",
  "imf_country", "imf_match", "heat_map_weights"
)

# Hides the backend-only heat columns from a table. `spatial_included` is a heat-only
# diagnostic too (heat rows are always "included"), but flood/other rows in a mixed table still
# need it, so it is hidden only when every row is a heat row.
drop_heat_backend_columns <- function(df) {
  df <- drop_drought_backend_columns(df)
  if (is.null(df) || !"excess_heat_days" %in% names(df)) {
    return(df)
  }
  is_heat_row <- if ("hazard_type" %in% names(df)) {
    !is.na(df$hazard_type) & df$hazard_type == "Heat"
  } else {
    !is.na(df$excess_heat_days)
  }
  drop_cols <- HEAT_BACKEND_ONLY_COLS
  if (nrow(df) > 0 && all(is_heat_row)) {
    drop_cols <- c(drop_cols, "spatial_included")
  }
  df |>
    dplyr::select(-dplyr::any_of(drop_cols))
}

# Global drought columns kept in the results object (backend) but not shown in the Asset
# Analysis tables: internal keys, calendar cell coordinates and per-season flags (the
# readable `growing_seasons` / `in_growing_period` columns are shown instead).
DROUGHT_BACKEND_ONLY_COLS <- c(
  "is_agriculture", "crop_key", "damage_crop", "calendar_lon", "calendar_lat",
  "gs_DJF", "gs_MAM", "gs_JJA", "gs_SON", "drought_map_weights"
)

drop_drought_backend_columns <- function(df) {
  if (is.null(df) || !"drought_status" %in% names(df)) {
    return(df)
  }
  df |>
    dplyr::select(-dplyr::any_of(DROUGHT_BACKEND_ONLY_COLS))
}

# Probabilistic-mode Asset Exposures table: unlike deterministic assets_factors
# (one row per asset per single event), probabilistic assets_factors has one row
# per asset PER RETURN PERIOD, with no per-row match to the UI's single event_id.
# This pivots the per-return-period diagnostics (flood depth, damage factor,
# business disruption) wide -- one column set per RP -- and joins in the
# integrated per-asset AAL (physical damage, business interruption) from
# results$aal_channels$summary, giving one row per asset.
build_probabilistic_assets_table <- function(assets_factors_by_rp, aal_summary, hazard_type_val = NULL) {
  if (is.null(assets_factors_by_rp) || nrow(assets_factors_by_rp) == 0) {
    return(tibble::tibble())
  }

  # With both a Flood and a Heat probabilistic event in one run, each event's panel
  # shows only its own hazard's rows and AAL columns.
  if (!is.null(hazard_type_val) && "hazard_type" %in% names(assets_factors_by_rp)) {
    assets_factors_by_rp <- assets_factors_by_rp |>
      dplyr::filter(.data$hazard_type == !!hazard_type_val)
    if (nrow(assets_factors_by_rp) == 0) {
      return(tibble::tibble())
    }
  }
  # Global drought (crop yield) AAL has its own layout: per-season SPEI-3 and the
  # season-averaged loss rate per return period.
  if ("drought_loss_rate" %in% names(assets_factors_by_rp) && any(!is.na(assets_factors_by_rp$drought_loss_rate))) {
    return(build_probabilistic_drought_table(
      assets_factors_by_rp[!is.na(assets_factors_by_rp$drought_loss_rate), , drop = FALSE],
      aal_summary
    ))
  }
  assets_factors_by_rp <- assets_factors_by_rp |>
    dplyr::select(-dplyr::any_of(c(
      "drought_loss_rate", paste0("spei3_", DROUGHT_SEASONS), "crop_input", "crop_key", "crop", "crop_source",
      "damage_crop", "calendar_crop", "calendar_match", "calendar_distance_km", "calendar_lon", "calendar_lat",
      paste0("gs_", DROUGHT_SEASONS), "growing_seasons", "is_agriculture", "off_window"
    )))

  # A flood-only table can still carry an all-NA excess_heat_days column when a heat event
  # was run alongside it (rows are bound across hazards), so test for actual heat values.
  is_heat_table <- "excess_heat_days" %in% names(assets_factors_by_rp) &&
    any(!is.na(assets_factors_by_rp$excess_heat_days))
  if (!is_heat_table) {
    assets_factors_by_rp <- assets_factors_by_rp |>
      dplyr::select(-dplyr::any_of(c(
        "excess_heat_days", "lp_exposure", "lp_loss", "nace_code", "nace_level", "nace_match",
        "nace_description", "nace_input", "imf_country", "imf_group", "imf_match", "beta_L", "beta_L_source",
        "s", "output_change_pct", "statistic"
      )))
  }

  depth_col <- if ("flood_depth_cm_raw" %in% names(assets_factors_by_rp)) {
    "flood_depth_cm_raw"
  } else if ("flood_depth_cm" %in% names(assets_factors_by_rp)) {
    "flood_depth_cm"
  } else {
    NA_character_
  }

  base_cols <- intersect(
    c("asset", "company", "asset_category", "share_of_economic_activity",
      if (is_heat_table) c("nace_code", "lp_exposure", "lp_loss", "statistic", "country", "imf_group", "beta_L", "beta_L_source")),
    names(assets_factors_by_rp)
  )
  asset_meta <- assets_factors_by_rp |>
    dplyr::select(dplyr::all_of(base_cols)) |>
    dplyr::distinct(.data$asset, .keep_all = TRUE)

  metric_cols <- intersect(
    if (is_heat_table) c("excess_heat_days", "output_change_pct") else c(depth_col, "damage_factor", "business_disruption"),
    names(assets_factors_by_rp)
  )
  metric_cols <- metric_cols[!is.na(metric_cols)]

  metric_labels <- c(
    excess_heat_days = "Excess Heat Days",
    output_change_pct = "Output Change (fraction)",
    flood_depth_cm_raw = "Flood Depth (cm)",
    flood_depth_cm = "Flood Depth (cm)",
    damage_factor = "Damage Factor",
    business_disruption = "Business Disruption (days)"
  )

  result <- asset_meta

  if (length(metric_cols) > 0 && "return_period" %in% names(assets_factors_by_rp)) {
    for (col in metric_cols) {
      wide_piece <- assets_factors_by_rp |>
        dplyr::select("asset", "return_period", value = dplyr::all_of(col)) |>
        dplyr::mutate(
          value = if (identical(col, "excess_heat_days")) {
                round(.data$value, 1)
              } else if (identical(col, "output_change_pct")) {
                round(.data$value, 6)
              } else {
                .data$value
              },
          rp_col = paste0(metric_labels[[col]], " (RP=", .data$return_period, ")")
        ) |>
        dplyr::select(-"return_period") |>
        tidyr::pivot_wider(names_from = "rp_col", values_from = "value")
      result <- result |> dplyr::left_join(wide_piece, by = "asset")
    }
  }

  if (!is.null(aal_summary) && is.data.frame(aal_summary) && nrow(aal_summary) > 0) {
    if (is_heat_table) {
      aal_display <- aal_summary |>
        dplyr::select(
          "asset",
          "AAL Labour Productivity - Revenue Rate (mean over years)" = "aal_lp_revenue_rate",
          dplyr::any_of(c("Average Annual Revenue Adjustment ($)" = "avg_annual_revenue_adjustment"))
        ) |>
        dplyr::mutate(
          dplyr::across(dplyr::any_of("AAL Labour Productivity - Revenue Rate (mean over years)"), ~ round(.x, 6)),
          dplyr::across(dplyr::any_of("Average Annual Revenue Adjustment ($)"), ~ round(.x, 0))
        )
    } else {
      aal_display <- aal_summary |>
        dplyr::select(-dplyr::any_of(c(
          "aal_lp_revenue_rate", "avg_annual_revenue_adjustment",
          "aal_drought_revenue_rate", "avg_annual_drought_revenue_adjustment"
        ))) |>
        dplyr::mutate(
          aal_pd_profit = paste0("$", formatC(round(.data$aal_pd_profit), format = "f", digits = 0, big.mark = ","))
        ) |>
        dplyr::rename(
          "AAL Physical Damage - Profit ($)" = "aal_pd_profit",
          "AAL Physical Damage - Revenue Rate" = "aal_pd_revenue_rate",
          "AAL Business Disruption - Revenue Rate" = "aal_bd_revenue_rate"
        )
    }
    result <- result |> dplyr::left_join(aal_display, by = "asset")
  }

  result
}

#' Probabilistic-mode Asset Exposures table for global Drought (internal)
#'
#' @description One row per agriculture asset: crop, growing seasons and calendar match,
#'   then per return period the SPEI-3 of each season (mean over the horizon) and the
#'   season-averaged yield-loss rate, then the drought AAL and revenue adjustment.
#' @noRd
build_probabilistic_drought_table <- function(drought_by_rp, aal_summary) {
  base_cols <- intersect(
    c("asset", "company", "asset_category", "share_of_economic_activity", "country",
      "crop_input", "crop", "crop_source", "calendar_crop", "growing_seasons", "calendar_match",
      "calendar_distance_km", "off_window", "statistic"),
    names(drought_by_rp)
  )
  result <- drought_by_rp |>
    dplyr::select(dplyr::all_of(base_cols)) |>
    dplyr::distinct(.data$asset, .keep_all = TRUE)
  if ("calendar_distance_km" %in% names(result)) {
    result$calendar_distance_km <- round(result$calendar_distance_km, 0)
  }
  result <- label_assumed_drought_crop(result)

  metric_cols <- intersect(c(paste0("spei3_", DROUGHT_SEASONS), "drought_loss_rate"), names(drought_by_rp))
  labels <- c(
    stats::setNames(paste0("SPEI-3 ", DROUGHT_SEASONS), paste0("spei3_", DROUGHT_SEASONS)),
    drought_loss_rate = "Yield Loss Rate (mean over seasons)"
  )
  for (col in metric_cols) {
    wide_piece <- drought_by_rp |>
      dplyr::select("asset", "return_period", value = dplyr::all_of(col)) |>
      dplyr::mutate(
        value = round(.data$value, if (identical(col, "drought_loss_rate")) 6 else 2),
        rp_col = paste0(labels[[col]], " (RP=", .data$return_period, ")")
      ) |>
      dplyr::select(-"return_period") |>
      tidyr::pivot_wider(names_from = "rp_col", values_from = "value")
    result <- result |> dplyr::left_join(wide_piece, by = "asset")
  }

  if (!is.null(aal_summary) && is.data.frame(aal_summary) && "aal_drought_revenue_rate" %in% names(aal_summary)) {
    aal_display <- aal_summary |>
      dplyr::select(
        "asset",
        "AAL Drought Yield Loss - Revenue Rate (mean over years)" = "aal_drought_revenue_rate",
        dplyr::any_of(c("Average Annual Revenue Adjustment ($)" = "avg_annual_drought_revenue_adjustment"))
      ) |>
      dplyr::mutate(
        dplyr::across(dplyr::any_of("AAL Drought Yield Loss - Revenue Rate (mean over years)"), ~ round(.x, 6)),
        dplyr::across(dplyr::any_of("Average Annual Revenue Adjustment ($)"), ~ round(.x, 0))
      )
    result <- result |> dplyr::left_join(aal_display, by = "asset")
  }
  result
}

#' results_assets Server Functions
#'
#' @param id Internal parameter for shiny
#' @param results_reactive reactive containing analysis results
#' @param name_mapping_reactive reactive containing region name mapping dictionary
#' @param cnae_exposure_reactive reactive returning CNAE exposure lookup table
#' @param events_reactive optional reactive containing the configured events snapshot used in the latest run
#' @param uncertainty_mode_reactive optional reactive returning logical indicating whether uncertainty mode is active
#' @param uncertainty_results_reactive optional reactive returning list(median, p10, p90) of result data frames
#' @param hazard_configs_reactive optional reactive returning hazard configuration list
#' @export
mod_results_assets_server <- function(id, results_reactive, name_mapping_reactive = NULL,
                                      cnae_exposure_reactive = NULL, events_reactive = NULL,
                                      uncertainty_mode_reactive = NULL,
                                      uncertainty_results_reactive = NULL,
                                      hazard_configs_reactive = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    resolve_cnae_exposure <- function() {
      if (is.null(cnae_exposure_reactive)) return(NULL)
      cnae_exposure_reactive()
    }

    get_uncertainty_mode <- function() {
      if (is.null(uncertainty_mode_reactive)) return(FALSE)
      isTRUE(uncertainty_mode_reactive())
    }

    # Look up the configured per-indicator aggregation method (e.g. flood_depth's
    # "agg" setting in Flood.yml / config_overrides.yml) for a given hazard type.
    resolve_indicator_aggregation <- function(hazard_type) {
      if (is.null(hazard_configs_reactive) || is.na(hazard_type)) return(NA_character_)
      cfg <- hazard_configs_reactive()
      if (is.null(cfg) || !hazard_type %in% names(cfg)) return(NA_character_)
      hazard_cfg <- cfg[[hazard_type]]
      indicator_key <- hazard_cfg$primary_indicator
      if (is.null(indicator_key) || !indicator_key %in% names(hazard_cfg$indicators)) {
        indicators <- hazard_cfg$indicators
        if (length(indicators) == 0) return(NA_character_)
        indicator_key <- names(indicators)[[1]]
      }
      agg <- hazard_cfg$indicators[[indicator_key]]$agg
      if (is.null(agg) || !nzchar(as.character(agg))) return(NA_character_)
      as.character(agg)
    }

    get_uncertainty_results <- function() {
      if (is.null(uncertainty_results_reactive)) return(NULL)
      uncertainty_results_reactive()
    }

    # Standard metadata columns that are NOT exposure metrics
    METADATA_COLS <- c(
      "asset", "company", "sector", "sector_name", "sector_code",
      "state", "state_code", "state_name", "province", "province_code",
      "municipality", "municipality_code", "municipality_name",
      "latitude", "longitude", "share_of_economic_activity",
      "asset_category", "asset_subtype", "size_in_m2", "cnae",
      "event_id", "hazard_name", "hazard_type", "hazard_indicator",
      "indicator_key", "hazard_key", "hazard_return_period",
      "return_period", "scenario_name", "season", "ensemble", "source",
      "matching_method", "spatial_included", "spatial_exposure_status",
      "spatial_multiplier", "spatial_level", "spatial_region_codes", "spatial_region_labels",
      "spatial_scheme", "event_year",
      "hazard_intensity", "damage_factor", "cost_factor"
    )

    # Identify exposure-metric columns (numeric, not in standard metadata)
    # growth_rate is a financial input column, not a hazard exposure metric
    EXCLUDE_FROM_UNCERTAINTY <- c("growth_rate", "discount_rate", "risk_free_rate")

    get_exposure_cols <- function(df) {
      if (is.null(df) || nrow(df) == 0) return(character(0))
      num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
      candidates <- setdiff(num_cols, c(METADATA_COLS, EXCLUDE_FROM_UNCERTAINTY))
      candidates[!grepl("_raw$", candidates)]
    }

    # Attach P10/Median/P90 exposure columns alongside the median values
    attach_uncertainty_exposure <- function(display_df, event_id_val, unc) {
      if (is.null(unc)) return(display_df)

      get_unc_assets <- function(results_obj) {
        af <- results_obj$assets_factors
        if (is.null(af) || nrow(af) == 0) return(tibble::tibble())
        if ("event_id" %in% names(af) && !is.na(event_id_val) && nzchar(as.character(event_id_val))) {
          af <- af |> dplyr::filter(as.character(.data$event_id) == as.character(event_id_val))
        }
        af
      }

      af_p10 <- get_unc_assets(unc$p10)
      af_med <- get_unc_assets(unc$median)
      af_p90 <- get_unc_assets(unc$p90)

      # Use median's assets_factors as the reference; display_df already has it formatted
      # Find the exposure columns present in the median result
      exp_cols <- get_exposure_cols(af_med)
      if (length(exp_cols) == 0) return(display_df)

      # Build a join key: just asset + event_id (unique per row typically)
      join_cols <- intersect(c("asset", "event_id"), names(af_med))
      if (length(join_cols) == 0 || !"asset" %in% names(display_df)) return(display_df)

      # Extract P10 and P90 exposure values
      pick_exp <- function(af, suffix) {
        if (nrow(af) == 0) return(NULL)
        keep <- intersect(c(join_cols, exp_cols), names(af))
        out <- af[, keep, drop = FALSE]
        # Rename exposure cols
        for (col in exp_cols) {
          if (col %in% names(out)) {
            names(out)[names(out) == col] <- paste0(col, suffix)
          }
        }
        out
      }

      p10_exp <- pick_exp(af_p10, "_P10")
      p90_exp <- pick_exp(af_p90, "_P90")

      result <- display_df
      if (!is.null(p10_exp)) {
        result <- dplyr::left_join(result, p10_exp, by = intersect(join_cols, names(result)))
      }
      if (!is.null(p90_exp)) {
        result <- dplyr::left_join(result, p90_exp, by = intersect(join_cols, names(result)))
      }

      # Reorder: for each exposure col, put median | P10 | P90 side by side
      other_cols <- setdiff(names(result), c(exp_cols, paste0(exp_cols, "_P10"), paste0(exp_cols, "_P90")))
      exp_ordered <- unlist(lapply(exp_cols, function(col) {
        c(col, paste0(col, "_P10"), paste0(col, "_P90"))
      }))
      exp_ordered <- intersect(exp_ordered, names(result))
      result[, c(other_cols, exp_ordered), drop = FALSE]
    }

    format_assets_table <- function(assets_df, name_mapping, cnae_exposure, include_sector_name = TRUE) {
      if (is.null(assets_df) || nrow(assets_df) == 0) {
        return(assets_df)
      }

      # Convert normalized province/state/municipality names back to original names for display
      if (!is.null(name_mapping)) {
        if ("province" %in% names(assets_df) && !is.null(name_mapping$province) && length(name_mapping$province) > 0) {
          province_lookup <- name_mapping$province
          assets_df <- assets_df |>
            dplyr::mutate(
              province = dplyr::if_else(
                !is.na(.data$province) & .data$province %in% names(province_lookup),
                province_lookup[.data$province],
                .data$province
              )
            )
        }

        if ("state" %in% names(assets_df) && !is.null(name_mapping$province) && length(name_mapping$province) > 0) {
          state_lookup <- name_mapping$province
          assets_df <- assets_df |>
            dplyr::mutate(
              state = dplyr::if_else(
                !is.na(.data$state) & .data$state %in% names(state_lookup),
                state_lookup[.data$state],
                .data$state
              )
            )
        }

        if ("municipality" %in% names(assets_df) && !is.null(name_mapping$municipality) && length(name_mapping$municipality) > 0) {
          municipality_lookup <- name_mapping$municipality
          assets_df <- assets_df |>
            dplyr::mutate(
              municipality = dplyr::if_else(
                !is.na(.data$municipality) & .data$municipality %in% names(municipality_lookup),
                municipality_lookup[.data$municipality],
                .data$municipality
              )
            )
        }
      }
      
      # Format state and municipality columns to show both code and name when available
      # This happens after name_mapping, so we use state_name/municipality_name which have original names
      if ("state_code" %in% names(assets_df) || "state_name" %in% names(assets_df)) {
        assets_df <- assets_df |>
          dplyr::mutate(
            state = dplyr::case_when(
              !is.na(.data$state_code) & !is.na(.data$state_name) ~ 
                paste0(.data$state_code, " - ", .data$state_name),
              !is.na(.data$state_code) ~ .data$state_code,
              !is.na(.data$state_name) ~ .data$state_name,
              TRUE ~ .data$state
            )
          )
      }
      
      if ("municipality_code" %in% names(assets_df) || "municipality_name" %in% names(assets_df)) {
        assets_df <- assets_df |>
          dplyr::mutate(
            municipality = dplyr::case_when(
              !is.na(.data$municipality_code) & !is.na(.data$municipality_name) ~ 
                paste0(.data$municipality_code, " - ", .data$municipality_name),
              !is.na(.data$municipality_code) ~ .data$municipality_code,
              !is.na(.data$municipality_name) ~ .data$municipality_name,
              TRUE ~ .data$municipality
            )
          )
      }

      numeric_cols <- vapply(assets_df, is.numeric, logical(1))
      numeric_col_names <- names(assets_df)[numeric_cols]
      for (col in numeric_col_names) {
        # Skip rounding for _raw columns to preserve exact extracted values
        if (grepl("_raw$", col)) {
          next
        }
        if (grepl("ratio|intensity", col)) {
          assets_df[[col]] <- round(assets_df[[col]], 4)
        } else if (grepl("cost|value", col)) {
          assets_df[[col]] <- round(assets_df[[col]], 0)
        }
      }

      if ("share_of_economic_activity" %in% names(assets_df)) {
        assets_df <- assets_df |>
          dplyr::mutate(
            share_of_economic_activity = dplyr::if_else(
              !is.na(.data$share_of_economic_activity),
              sprintf("%.1f%%", .data$share_of_economic_activity * 100),
              NA_character_
            )
          )
      }

      # Global heat rows: show the matched NACE activity as the sector (the CNAE lookup below
      # would mislabel NACE codes) and keep the heat columns readable.
      if ("excess_heat_days" %in% names(assets_df)) {
        assets_df <- assets_df |>
          dplyr::mutate(
            excess_heat_days = round(.data$excess_heat_days, 1),
            lp_loss = round(.data$lp_loss, 4),
            dplyr::across(dplyr::any_of("output_change_pct"), ~ round(.x, 6))
          )
      }
      # Global drought rows: readable SPEI-3 / distance values
      if ("drought_status" %in% names(assets_df)) {
        assets_df <- assets_df |>
          dplyr::mutate(
            dplyr::across(dplyr::any_of(c("spei3", "spei3_matched")), ~ round(.x, 2)),
            dplyr::across(dplyr::any_of("calendar_distance_km"), ~ round(.x, 0)),
            dplyr::across(dplyr::any_of(c("damage_factor", "damage_factor_in_window")), ~ round(.x, 6))
          ) |>
          label_assumed_drought_crop()
      }
      heat_sector <- if ("nace_description" %in% names(assets_df)) assets_df$nace_description else NULL

      original_has_sector <- "sector" %in% names(assets_df)

      assets_df <- attach_sector_metadata(assets_df, cnae_exposure)
      if (!is.null(heat_sector)) {
        existing_sector <- if ("sector" %in% names(assets_df)) {
          as.character(assets_df$sector)
        } else {
          rep(NA_character_, nrow(assets_df))
        }
        assets_df$sector <- dplyr::coalesce(heat_sector, existing_sector)
        assets_df <- assets_df |>
          dplyr::select(-dplyr::any_of(c("nace_description", "nace_input", "country_input", "sector_name", "sector_code")))
        original_has_sector <- TRUE
      }

      assets_df <- if (!is.null(heat_sector)) {
        assets_df
      } else if (original_has_sector) {
        assets_df |>
          dplyr::mutate(
            sector = dplyr::coalesce(
              .data$sector_name,
              dplyr::if_else(
                !is.na(as.character(.data$sector)) & !grepl("^[0-9]+$", as.character(.data$sector)),
                as.character(.data$sector),
                NA_character_
              ),
              as.character(.data$sector)
            )
          )
      } else {
        assets_df |>
          dplyr::mutate(
            sector = .data$sector_name
          )
      }

      assets_df <- assets_df |>
        dplyr::select(-dplyr::any_of("cnae"))

      # For non-agriculture assets, subtype is hazard-specific fallback (e.g. "Assumed Soybean")
      # and should not be shown — blank it out so rows deduplicate cleanly
      if ("asset_subtype" %in% names(assets_df) && "asset_category" %in% names(assets_df)) {
        assets_df <- assets_df |>
          dplyr::mutate(
            asset_subtype = dplyr::if_else(
              !is.na(.data$asset_category) & tolower(as.character(.data$asset_category)) == "agriculture",
              .data$asset_subtype,
              NA_character_
            )
          )
      }

      # Remove internal keys and raw extraction columns from display
      assets_df <- assets_df |>
        dplyr::select(-dplyr::any_of(c("indicator_key", "hazard_key", "hazard_indicator"))) |>
        dplyr::select(-dplyr::matches("_raw$")) |>
        drop_heat_backend_columns()

      if (!include_sector_name) {
        assets_df <- assets_df |>
          dplyr::select(-dplyr::any_of("sector_name"))
      }

      priority_cols <- c(
        "asset",
        "company",
        "sector",
        "sector_name",
        "sector_code",
        "state",
        "state_code",
        "state_name",
        "province",
        "province_code",
        "municipality",
        "municipality_code",
        "municipality_name",
        "share_of_economic_activity",
        "event_id",
        "hazard_name",
        "hazard_type",
        "matching_method",
        "spatial_exposure_status",
        "spatial_multiplier",
        "hazard_return_period",
        "event_year",
        "damage_factor",
        "statistic",
        "season",
        "crop",
        "crop_source",
        "growing_seasons",
        "in_growing_period",
        "spei3",
        "spei3_matched",
        "damage_factor_in_window",
        "off_window",
        "drought_status",
        "calendar_crop",
        "calendar_match",
        "calendar_distance_km",
        "drought_revenue_adjustment",
        "excess_heat_days",
        "lp_exposure",
        "lp_loss",
        "nace_code",
        "country",
        "imf_group",
        "beta_L",
        "beta_L_source",
        "output_change_pct"
      )
      existing_priority <- intersect(priority_cols, names(assets_df))
      other_cols <- setdiff(names(assets_df), existing_priority)

      if (length(existing_priority) > 0) {
        assets_df <- assets_df[, c(existing_priority, other_cols), drop = FALSE]
      }

      assets_df
    }

    drop_empty_columns <- function(df) {
      if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) {
        return(df)
      }

      cols_to_keep <- purrr::map_lgl(names(df), function(col_name) {
        col_data <- df[[col_name]]
        
        # For character columns, check for NA or empty strings
        if (is.character(col_data)) {
          # Check if there's at least one non-NA, non-empty value
          has_content <- !is.na(col_data) & nzchar(col_data) > 0
          any(has_content)
        } else {
          # For other types, check for at least one non-NA value
          !all(is.na(col_data))
        }
      })

      df[, cols_to_keep, drop = FALSE]
    }

    resolve_run_events <- function() {
      if (is.null(events_reactive)) {
        return(NULL)
      }

      events <- try(events_reactive(), silent = TRUE)
      if (inherits(events, "try-error") || is.null(events) || nrow(events) == 0) {
        return(NULL)
      }

      events
    }

    normalize_events_metadata <- function(events_df) {
      if (is.null(events_df) || nrow(events_df) == 0) {
        return(tibble::tibble())
      }

      required_cols <- c(
        "event_id",
        "hazard_type",
        "hazard_name",
        "scenario_name",
        "return_period",
        "event_year",
        "spatial_level",
        "spatial_region_codes",
        "spatial_region_labels",
        "calc_mode"
      )

      out <- events_df
      for (col_name in required_cols) {
        if (!col_name %in% names(out)) {
          out[[col_name]] <- NA
        }
      }

      out <- out |>
        dplyr::select(dplyr::all_of(required_cols)) |>
        dplyr::mutate(
          event_id = as.character(.data$event_id),
          return_period = suppressWarnings(as.numeric(.data$return_period)),
          event_year = suppressWarnings(as.integer(.data$event_year)),
          calc_mode = dplyr::coalesce(as.character(.data$calc_mode), "deterministic")
        ) |>
        dplyr::filter(!is.na(.data$event_id) & nzchar(.data$event_id)) |>
        dplyr::distinct(.data$event_id, .keep_all = TRUE)

      out
    }

    extract_assets_event_metadata <- function(assets_df) {
      if (is.null(assets_df) || nrow(assets_df) == 0 || !"event_id" %in% names(assets_df)) {
        return(tibble::tibble())
      }

      metadata_cols <- c(
        "event_id",
        "hazard_type",
        "hazard_name",
        "scenario_name",
        "return_period",
        "hazard_return_period",
        "event_year",
        "spatial_level",
        "spatial_region_codes",
        "spatial_region_labels"
      )

      out <- assets_df |>
        dplyr::select(dplyr::any_of(metadata_cols)) |>
        dplyr::mutate(event_id = as.character(.data$event_id)) |>
        dplyr::filter(!is.na(.data$event_id) & nzchar(.data$event_id)) |>
        dplyr::distinct(.data$event_id, .keep_all = TRUE)

      if ("hazard_return_period" %in% names(out)) {
        if ("return_period" %in% names(out)) {
          out$return_period <- dplyr::coalesce(
            suppressWarnings(as.numeric(out$return_period)),
            suppressWarnings(as.numeric(out$hazard_return_period))
          )
        } else {
          out$return_period <- suppressWarnings(as.numeric(out$hazard_return_period))
        }
      }

      out |>
        dplyr::select(-dplyr::any_of("hazard_return_period"))
    }

    merge_event_metadata <- function(run_events_df, assets_event_meta) {
      if (nrow(run_events_df) == 0) {
        return(assets_event_meta)
      }
      if (nrow(assets_event_meta) == 0) {
        return(run_events_df)
      }

      merged <- run_events_df |>
        dplyr::left_join(assets_event_meta, by = "event_id", suffix = c("", ".asset"))

      merge_cols <- c(
        "hazard_type",
        "hazard_name",
        "scenario_name",
        "return_period",
        "event_year",
        "spatial_level",
        "spatial_region_codes",
        "spatial_region_labels"
      )

      for (col_name in merge_cols) {
        asset_col <- paste0(col_name, ".asset")
        if (asset_col %in% names(merged)) {
          if (!col_name %in% names(merged)) {
            merged[[col_name]] <- merged[[asset_col]]
          } else {
            merged[[col_name]] <- dplyr::coalesce(merged[[col_name]], merged[[asset_col]])
          }
          merged[[asset_col]] <- NULL
        }
      }

      merged
    }

    build_event_panels <- function(assets_df, run_events_df = NULL) {
      assets_event_meta <- extract_assets_event_metadata(assets_df)
      run_events_meta <- normalize_events_metadata(run_events_df)

      panel_events <- if (nrow(run_events_meta) > 0) {
        merge_event_metadata(run_events_meta, assets_event_meta)
      } else {
        assets_event_meta
      }

      if (nrow(panel_events) == 0) {
        return(panel_events)
      }

      panel_events |>
        dplyr::mutate(
          hazard_type_label = dplyr::if_else(
            !is.na(.data$hazard_type) & nzchar(as.character(.data$hazard_type)),
            as.character(.data$hazard_type),
            "Unknown hazard"
          ),
          event_year_label = dplyr::if_else(
            !is.na(.data$event_year),
            as.character(.data$event_year),
            "NA"
          ),
          panel_label = paste0(.data$event_id, " | ", .data$hazard_type_label, " | ", .data$event_year_label),
          spatial_selection = mapply(
            FUN = format_spatial_selection,
            level = .data$spatial_level,
            region_codes = .data$spatial_region_codes,
            region_labels = .data$spatial_region_labels,
            SIMPLIFY = TRUE,
            USE.NAMES = FALSE
          )
        )
    }

    format_event_metadata_value <- function(value) {
      if (is.null(value) || length(value) == 0) {
        return("N/A")
      }

      val <- value[[1]]
      if (is.numeric(val)) {
        if (is.na(val)) {
          return("N/A")
        }
        if (isTRUE(all.equal(val, round(val)))) {
          return(as.character(as.integer(round(val))))
        }
        return(as.character(val))
      }

      val_chr <- as.character(val)
      if (is.na(val_chr) || !nzchar(trimws(val_chr))) {
        return("N/A")
      }

      val_chr
    }

    build_event_metadata_ui <- function(event_row) {
      calc_mode_val <- if ("calc_mode" %in% names(event_row)) event_row$calc_mode[[1]] else "deterministic"
      mode_label <- if (identical(calc_mode_val, "probabilistic")) "Probabilistic (AAL)" else "Deterministic"

      metadata_items <- list(
        list(label = "Event ID", value = event_row$event_id),
        list(label = "Mode", value = mode_label),
        list(label = "Hazard Type", value = event_row$hazard_type),
        list(label = "Hazard Name", value = event_row$hazard_name),
        list(label = "Scenario", value = event_row$scenario_name),
        list(label = "Return Period (years)", value = event_row$return_period),
        list(label = "Shock Year", value = event_row$event_year),
        list(label = "Spatial Separation", value = event_row$spatial_selection)
      )

      shiny::tags$div(
        class = "event-panel-metadata text-muted",
        style = "margin-bottom: 0.75rem; font-size: 0.9em;",
        purrr::map(metadata_items, function(item) {
          shiny::tags$div(
            shiny::tags$strong(paste0(item$label, ": ")),
            format_event_metadata_value(item$value)
          )
        })
      )
    }

    # Transform the ensemble/aggregation metadata column for display:
    # - Flood: no ensemble dimension → rename to "aggregation", show agg method
    # - NC hazards: keep "ensemble", show selected value
    # - When uncertainty mode ON: show "p10 / median / p90" for both
    transform_hazard_source_column <- function(df, hazard_type, uncertainty_mode, aggregation_method = "mean") {
      if (is.null(df) || nrow(df) == 0) return(df)

      is_flood <- !is.na(hazard_type) && tolower(as.character(hazard_type)) == "flood"

      if (is_flood) {
        # Rename ensemble → aggregation and set value
        if ("ensemble" %in% names(df)) {
          names(df)[names(df) == "ensemble"] <- "aggregation"
        } else if (!"aggregation" %in% names(df)) {
          df$aggregation <- NA_character_
        }
        df$aggregation <- if (uncertainty_mode) "p10 / median / p90" else aggregation_method
      } else {
        # NC hazard: update ensemble column value
        if ("ensemble" %in% names(df)) {
          if (uncertainty_mode) {
            df$ensemble <- "p10 / median / p90"
          }
          # If not uncertainty mode, keep the actual value already in the column
        }
      }

      df
    }

    drop_event_columns_for_display <- function(df) {
      if (is.null(df) || nrow(df) == 0) {
        return(df)
      }

      event_level_cols <- c(
        "event_id",
        "hazard_name",
        "hazard_type",
        "scenario_name",
        "return_period",
        "hazard_return_period",
        "event_year",
        "spatial_level",
        "spatial_region_codes",
        "spatial_region_labels",
        "spatial_scheme",
        "spatial_selection",
        "state_code",
        "state_name",
        "municipality_code",
        "municipality_name"
      )

      df |>
        dplyr::select(-dplyr::any_of(event_level_cols))
    }

    output$hazard_tables <- shiny::renderUI({
      results <- results_reactive()
      if (is.null(results)) {
        return(shiny::wellPanel(shiny::p("Asset results will appear here once the analysis completes.")))
      }

      assets <- if (!is.null(results$assets_factors)) results$assets_factors else tibble::tibble()
      panel_events <- build_event_panels(assets, resolve_run_events())

      if (nrow(panel_events) == 0) {
        return(shiny::wellPanel(shiny::p("No events available for display.")))
      }

      event_blocks <- purrr::map(seq_len(nrow(panel_events)), function(idx) {
        event_row <- panel_events[idx, , drop = FALSE]
        table_output <- DT::dataTableOutput(ns(paste0("assets_table_", idx)))
        metadata_output <- build_event_metadata_ui(event_row)

        if (idx == 1) {
          shiny::tags$details(
            class = "hazard-panel",
            open = NA,
            shiny::tags$summary(class = "hazard-panel__summary", event_row$panel_label[[1]]),
            shiny::tags$div(class = "hazard-panel__table", metadata_output, table_output)
          )
        } else {
          shiny::tags$details(
            class = "hazard-panel",
            shiny::tags$summary(class = "hazard-panel__summary", event_row$panel_label[[1]]),
            shiny::tags$div(class = "hazard-panel__table", metadata_output, table_output)
          )
        }
      })

      shiny::tagList(event_blocks)
    })

    shiny::observe({
      results <- results_reactive()
      if (is.null(results)) {
        session$userData$hazard_tables_data <- NULL
        session$userData$hazard_tables_display_data <- NULL
        return(NULL)
      }

      assets <- if (!is.null(results$assets_factors)) results$assets_factors else tibble::tibble()
      panel_events <- build_event_panels(assets, resolve_run_events())

      if (nrow(panel_events) == 0) {
        session$userData$hazard_tables_data <- NULL
        session$userData$hazard_tables_display_data <- NULL
        return(NULL)
      }

      session$userData$hazard_tables_data <- vector("list", length = nrow(panel_events))
      session$userData$hazard_tables_display_data <- vector("list", length = nrow(panel_events))

      purrr::walk(seq_len(nrow(panel_events)), function(idx) {
        event_row <- panel_events[idx, , drop = FALSE]
        event_id_val <- event_row$event_id[[1]]

        output[[paste0("assets_table_", idx)]] <- DT::renderDataTable({
          calc_mode_val <- if ("calc_mode" %in% names(event_row)) event_row$calc_mode[[1]] else "deterministic"

          if (identical(calc_mode_val, "probabilistic")) {
            # Probabilistic assets_factors has one row per asset PER RETURN
            # PERIOD (not one row per event_id), so it isn't filtered by
            # event_id_val at all -- every row already belongs to this one
            # scenario/hazard_type by construction (see compute_risk_probabilistic()).
            probabilistic_table <- build_probabilistic_assets_table(
              assets, results$aal_channels$summary,
              hazard_type_val = if ("hazard_type" %in% names(event_row)) as.character(event_row$hazard_type[[1]]) else NULL
            )
            session$userData$hazard_tables_data[[idx]] <- probabilistic_table
            session$userData$hazard_tables_display_data[[idx]] <- probabilistic_table

            if (is.null(probabilistic_table) || nrow(probabilistic_table) == 0) {
              return(DT::datatable(
                tibble::tibble(Message = "No assets available for this event."),
                options = list(dom = "t"),
                rownames = FALSE
              ))
            }

            return(DT::datatable(
              probabilistic_table,
              options = list(pageLength = 25, scrollX = TRUE),
              rownames = FALSE
            ))
          }

          current_assets <- assets

          if ("event_id" %in% names(current_assets) && !is.na(event_id_val) && nzchar(as.character(event_id_val))) {
            current_assets <- current_assets |>
              dplyr::filter(as.character(.data$event_id) == as.character(event_id_val))
          }

          name_mapping <- if (!is.null(name_mapping_reactive)) name_mapping_reactive() else NULL
          cnae_exposure <- resolve_cnae_exposure()
          formatted_assets <- format_assets_table(current_assets, name_mapping, cnae_exposure, include_sector_name = FALSE)
          session$userData$hazard_tables_data[[idx]] <- formatted_assets

          if (is.null(formatted_assets) || nrow(formatted_assets) == 0) {
            session$userData$hazard_tables_display_data[[idx]] <- tibble::tibble()
            return(DT::datatable(
              tibble::tibble(Message = "No assets available for this event."),
              options = list(dom = "t"),
              rownames = FALSE
            ))
          }

          hazard_type_val <- if ("hazard_type" %in% names(event_row)) event_row$hazard_type[[1]] else NA_character_
          uncertainty_on  <- get_uncertainty_mode()
          agg_method_val  <- resolve_indicator_aggregation(hazard_type_val)
          if (is.na(agg_method_val)) {
            agg_method_val <- if (!is.null(results$aggregation_method)) results$aggregation_method else "mean"
          }

          display_assets <- formatted_assets |>
            drop_event_columns_for_display() |>
            (\(df) transform_hazard_source_column(df, hazard_type_val, uncertainty_on, agg_method_val))()

          if (uncertainty_on) {
            unc <- get_uncertainty_results()
            display_assets <- attach_uncertainty_exposure(display_assets, event_id_val, unc)
            # For drought, lower SPI = more negative = higher risk, so hazard P10 = risk P90
            is_drought <- !is.na(hazard_type_val) && tolower(as.character(hazard_type_val)) == "drought"
            if (is_drought) {
              p10_cols <- grep("_P10$", names(display_assets), value = TRUE)
              p90_cols <- gsub("_P10$", "_P90", p10_cols)
              both_exist <- p90_cols %in% names(display_assets)
              for (i in seq_along(p10_cols)[both_exist]) {
                tmp <- display_assets[[p10_cols[i]]]
                display_assets[[p10_cols[i]]] <- display_assets[[p90_cols[i]]]
                display_assets[[p90_cols[i]]] <- tmp
              }
            }
          }

          display_assets <- drop_empty_columns(display_assets)

          session$userData$hazard_tables_display_data[[idx]] <- display_assets

          DT::datatable(
            display_assets,
            options = list(
              pageLength = 25,
              scrollX = TRUE
            ),
            rownames = FALSE
          )
        })
      })
    })

    assets_download_data <- shiny::reactive({
      results <- results_reactive()
      if (is.null(results) || is.null(results$assets_factors) || nrow(results$assets_factors) == 0) {
        return(NULL)
      }

      name_mapping <- if (!is.null(name_mapping_reactive)) name_mapping_reactive() else NULL
      format_assets_table(results$assets_factors, name_mapping, resolve_cnae_exposure())
    })

    output$download_assets_csv <- shiny::downloadHandler(
      filename = function() {
        paste0("asset_results_", Sys.Date(), ".csv")
      },
      content = function(file) {
        data <- assets_download_data()
        if (is.null(data) || nrow(data) == 0) {
          utils::write.csv(data.frame(message = "No asset results available"), file, row.names = FALSE)
        } else {
          utils::write.csv(as.data.frame(data), file, row.names = FALSE)
        }
      }
    )

    output$download_assets_excel <- shiny::downloadHandler(
      filename = function() {
        paste0("asset_results_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        data <- assets_download_data()
        if (is.null(data) || nrow(data) == 0) {
          writexl::write_xlsx(data.frame(message = "No asset results available"), path = file)
        } else {
          writexl::write_xlsx(as.data.frame(data), path = file)
        }
      }
    )
  })
}
