#' hazards_events UI Function
#'
#' @description Shiny module to build a list of hazard events to apply. Allows adding
#' multiple events with type, hazard name, and event year.
#' @param id Internal parameter for shiny
#' @param title Character title displayed above the controls
#' @export
mod_hazards_events_ui <- function(id, title = "Hazard events") {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4(title),
    shiny::p("Select at least one hazard event to run the analysis:", class = "text-muted", style = "font-size: 0.9em; margin-bottom: 10px;"),
    shiny::uiOutput(ns("events_ui")),
    shiny::div(
      style = "display: flex; gap: 8px; align-items: center; justify-content: center; flex-wrap: wrap; margin-top: 15px; margin-bottom: 10px;",
      shiny::tags$label(
        `for` = ns("upload_hazard_config"),
        class = "btn btn-outline-secondary btn-sm",
        style = "margin: 0; cursor: pointer;",
        shiny::icon("upload"),
        shiny::tags$span(class = "d-none d-sm-inline", " Load Events")
      ),
      shiny::tags$a(
        id = ns("download_config"),
        class = "btn btn-outline-secondary btn-sm shiny-download-link",
        href = "",
        target = "_blank",
        download = NA,
        shiny::icon("download"),
        shiny::tags$span(class = "d-none d-sm-inline", " Save Events")
      )
    ),
    shiny::tags$input(
      id = ns("upload_hazard_config"),
      type = "file",
      accept = ".xlsx,.xls",
      style = "display: none;"
    )
  )
}

#' hazards_events Server Functions
#'
#' @param id Internal parameter for shiny
#' @param hazards_inventory reactive data.frame with columns: hazard_type, hazard_indicator, scenario_name, return_period, hazard_name
#' @param hazard_configs reactive list from load_hazards_and_inventory()$configs
#' @param base_dir_reactive optional reactive returning base_dir for loading spatial separation options
#' @param spatial_data_reactive optional reactive returning preloaded spatial separation data
#' @param region_reactive optional reactive returning "brazil" (default) or "global"; when "global",
#'   the per-event Spatial Separation panel is hidden (no sub-region breakdown in Global mode)
#' @return reactive data.frame of configured events with columns: event_id, hazard_type, hazard_indicator, hazard_name, scenario_name, return_period, event_year, season, calc_mode
#' @export
mod_hazards_events_server <- function(id, hazards_inventory, hazard_configs, base_dir_reactive = NULL, spatial_data_reactive = NULL, region_reactive = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
      events_rv <- shiny::reactiveVal(tibble::tibble(
      event_id = character(),
      hazard_type = character(),
      hazard_indicator = character(),
      hazard_name = character(),
      scenario_name = character(),
      return_period = numeric(),
      event_year = integer(),
      season = character(),
      spatial_level = character(),
      spatial_region_codes = character(),
      spatial_region_labels = character(),
      spatial_scheme = character(),
      # "deterministic" (default, single event-year shock) or "probabilistic"
      # (Average Annual Loss across every available return period, deducted
      # from every year of the pathway). See calculate_aal() / Phase 3.
      calc_mode = character()
    ))

    # Counter for dynamic UIs
    counter <- shiny::reactiveVal(1L)

    # Hazard types with a working Probabilistic/AAL calculation engine: Flood, plus the
    # global heat and global drought hazards (Global mode). The Mode toggle is shown for every hazard, but
    # Probabilistic is only actually honored for these -- picking it for anything else is
    # rejected at Add time with a clear message, so the dropdown itself never needs
    # hazard-type-dependent filtering.
    probabilistic_capable_hazards_for <- function(cfg) {
      c("Flood", global_heat_hazard_types(cfg), global_drought_hazard_types(cfg))
    }

    # Shared event_id assignment, used by both the deterministic and probabilistic
    # add_event branches so the two paths never diverge on this bookkeeping detail.
    next_event_id <- function(cur_events) {
      if (nrow(cur_events) > 0 && "event_id" %in% names(cur_events)) {
        existing_ids <- cur_events$event_id
        numeric_ids <- as.integer(gsub("^ev", "", existing_ids))
        numeric_ids <- numeric_ids[!is.na(numeric_ids)]
        if (length(numeric_ids) > 0) max(numeric_ids) + 1L else 1L
      } else {
        1L
      }
    }

    spatial_data <- shiny::reactive({
      if (!is.null(spatial_data_reactive)) {
        preloaded <- try(spatial_data_reactive(), silent = TRUE)
        if (!inherits(preloaded, "try-error") && !is.null(preloaded)) {
          return(preloaded)
        }
      }
      if (is.null(base_dir_reactive)) {
        return(NULL)
      }
      base_dir <- base_dir_reactive()
      if (is.null(base_dir) || !nzchar(base_dir) || !dir.exists(base_dir)) {
        return(NULL)
      }
      loaded <- try(load_spatial_separation_data(base_dir), silent = TRUE)
      if (inherits(loaded, "try-error")) {
        return(NULL)
      }
      loaded
    })

    # Create UI-friendly inventory (only primary indicators)
    ui_inventory <- shiny::reactive({
      inv <- try(hazards_inventory(), silent = TRUE)
      if (inherits(inv, "try-error") || is.null(inv) || nrow(inv) == 0) {
        return(tibble::tibble())
      }
      cfg <- try(hazard_configs(), silent = TRUE)
      if (inherits(cfg, "try-error") || is.null(cfg)) {
        return(tibble::tibble())
      }
      filter_inventory_for_ui(inv, cfg)
    })

    # Add event button
    shiny::observeEvent(input$add_event, {
      k <- counter()
      haz_type <- input[[paste0("hazard_type_", k)]]
      if (is.null(haz_type)) return()

      calc_mode_val <- input[[paste0("calc_mode_", k)]]
      if (is.null(calc_mode_val) || !nzchar(as.character(calc_mode_val))) {
        calc_mode_val <- "deterministic"
      }
      calc_mode_val <- as.character(calc_mode_val)

      cfg <- try(hazard_configs(), silent = TRUE)
      if (inherits(cfg, "try-error")) cfg <- NULL

      # ------------------------------------------------------------------
      # PROBABILISTIC (AAL) BRANCH -- kept fully separate from the
      # deterministic branch below so toggling modes can never corrupt the
      # deterministic single-event-year logic. Only requires scenario_name
      # (every available return period is pulled in at calculation time,
      # inside the new AAL engine -- not here).
      # ------------------------------------------------------------------
      if (identical(calc_mode_val, "probabilistic")) {
        probabilistic_capable_hazards <- probabilistic_capable_hazards_for(cfg)
        if (!haz_type %in% probabilistic_capable_hazards) {
          message(
            "[mod_hazards_events] Probabilistic mode is not yet supported for hazard type: ",
            haz_type, ". Supported: ", paste(probabilistic_capable_hazards, collapse = ", ")
          )
          counter(k + 1L)
          return()
        }

        scenario_val <- input[[paste0("filter_", k, "_scenario_name")]]
        if (is.null(scenario_val) || !nzchar(as.character(scenario_val))) {
          counter(k + 1L)
          return()
        }

        hazard_indicator_val <- get_index_indicator(cfg, haz_type)
        if (is.na(hazard_indicator_val)) {
          counter(k + 1L)
          return()
        }

        # Hazards whose index includes a statistic (global heat) carry it on the event.
        prob_index_cols <- cfg[[haz_type]]$indicators[[hazard_indicator_val]]$index
        statistic_val <- NULL
        if ("statistic" %in% prob_index_cols) {
          statistic_val <- input[[paste0("filter_", k, "_statistic")]]
          if (is.null(statistic_val) || !nzchar(as.character(statistic_val))) statistic_val <- "median"
          statistic_val <- as.character(statistic_val)
        }

        cur_events <- events_rv()
        next_id <- next_event_id(cur_events)

        new_row <- tibble::tibble(
          event_id = paste0("ev", next_id),
          hazard_type = haz_type,
          hazard_indicator = hazard_indicator_val,
          # No single hazard_name exists for an "all return periods" event --
          # this is a descriptive label only, never used to look up one raster.
          hazard_name = paste0(
            haz_type, "__AAL__scenario_name=", as.character(scenario_val),
            if (!is.null(statistic_val)) paste0("__statistic=", statistic_val) else ""
          ),
          scenario_name = as.character(scenario_val),
          return_period = NA_real_,
          event_year = NA_integer_,
          season = NA_character_,
          # Spatial separation is not implemented for Probabilistic/AAL in v1
          # (see plan); the whole-area sentinel ("brazil" / "global") is a genuine
          # no-op regardless of region.
          spatial_level = if (identical(if (!is.null(region_reactive)) region_reactive() else "brazil", "global")) {
            GLOBAL_WHOLE_AREA_LEVEL
          } else {
            "brazil"
          },
          spatial_region_codes = NA_character_,
          spatial_region_labels = NA_character_,
          spatial_scheme = NA_character_,
          calc_mode = "probabilistic"
        )
        if (!is.null(statistic_val)) new_row$statistic <- statistic_val

        events_rv(dplyr::bind_rows(cur_events, new_row))
        counter(k + 1L)
        return()
      }

      # ------------------------------------------------------------------
      # DETERMINISTIC BRANCH -- unchanged from the existing single-event
      # behavior; only the final calc_mode tag (below) is new.
      # ------------------------------------------------------------------

      # Get index configuration for this hazard type
      index_ind <- get_index_indicator(cfg, haz_type)
      index_cols <- cfg[[haz_type]]$indicators[[index_ind]]$index

      # Collect all index values from dynamic inputs
      index_values <- list()
      for (idx_col in index_cols) {
        val <- input[[paste0("filter_", k, "_", idx_col)]]
        if (is.null(val)) {
          # If a required filter is missing, increment counter and return
          counter(k + 1L)
          return()
        }
        index_values[[idx_col]] <- val
      }

      full_inv <- try(hazards_inventory(), silent = TRUE)
      if (inherits(full_inv, "try-error")) {
        full_inv <- NULL
      }

      lookup <- lookup_hazard_entry(
        hazard_type_val = haz_type,
        index_values = index_values,
        hazard_configs = cfg,
        hazards_inventory = full_inv
      )

      hazard_indicator_val <- lookup$hazard_indicator
      hazard_name_val <- lookup$hazard_name

      if (is.na(hazard_indicator_val) || is.na(hazard_name_val) || hazard_name_val == "") {
        message(
          "[mod_hazards_events] Could not determine hazard metadata for: ",
          haz_type, " with indices ", paste(names(index_values), index_values, sep="=", collapse=", ")
        )
        counter(k + 1L)
        return()
      }

      # Prepare new row - start with basic columns
      # Generate unique event_id based on existing events to avoid duplicates
      cur_events <- events_rv()
      next_id <- next_event_id(cur_events)

      new_row <- tibble::tibble(
        event_id = paste0("ev", next_id),
        hazard_type = haz_type,
        hazard_indicator = hazard_indicator_val,
        hazard_name = hazard_name_val,
        event_year = as.integer(input[[paste0("year_", k)]])
      )
      
      # Add all index columns to the new row
      for (idx_col in index_cols) {
        val <- index_values[[idx_col]]
        if (idx_col == "return_period") val <- as.numeric(val)
        new_row[[idx_col]] <- val
      }
      
      # Ensure backward compatibility columns exist in events table for downstream pipeline
      # 1. Map 'gwl' to 'scenario_name' if needed
      if (!"scenario_name" %in% names(new_row)) {
        if ("gwl" %in% names(new_row)) {
          new_row$scenario_name <- new_row$gwl
        } else {
          new_row$scenario_name <- NA_character_
        }
      }
      
      # 2. Map 'return_period' (if it was called something else in index, though unlikely)
      if (!"return_period" %in% names(new_row)) {
        new_row$return_period <- NA_real_
      }
      
      # 3. Ensure season exists
      if (!"season" %in% names(new_row)) new_row$season <- NA_character_

      add_region_val <- if (!is.null(region_reactive)) region_reactive() else "brazil"
      add_is_global <- identical(add_region_val, "global")
      # Global Flood uses the global basin scheme and Global Heat / Drought the
      # administrative-region scheme; every other hazard keeps its configured scheme.
      add_is_global_heat <- add_is_global && haz_type %in% global_adm_hazard_types(cfg)
      spatial_scheme_val <- if (add_is_global && identical(haz_type, "Flood")) {
        "global_basins"
      } else if (add_is_global_heat) {
        "global_adm"
      } else {
        get_hazard_spatial_scheme(cfg, haz_type)
      }
      spatial_level_val <- input[[paste0("spatial_level_", k)]]
      if (is.null(spatial_level_val) || !nzchar(as.character(spatial_level_val))) {
        spatial_level_val <- if (add_is_global) GLOBAL_WHOLE_AREA_LEVEL else "brazil"
      }
      spatial_level_val <- tolower(as.character(spatial_level_val))

      selected_region_codes <- input[[paste0("spatial_regions_", k)]]
      if (is.null(selected_region_codes)) selected_region_codes <- character(0)
      selected_region_codes <- as.character(selected_region_codes)
      selected_region_codes <- selected_region_codes[nzchar(selected_region_codes)]
      if (spatial_level_val %in% whole_area_spatial_levels()) {
        selected_region_codes <- character(0)
      }

      selected_region_labels <- character(0)
      if (identical(spatial_scheme_val, "global_adm")) {
        # level -> countries -> regions: resolves to the explicit level / codes / labels stored on the
        # event (countries without regions are stored as the countries themselves, see the helper)
        adm_sel <- build_global_adm_selection(
          level = spatial_level_val,
          countries = input[[paste0("spatial_countries_", k)]],
          regions = selected_region_codes,
          spatial_data = spatial_data()
        )
        spatial_level_val <- adm_sel$level
        selected_region_codes <- adm_sel$codes
        selected_region_labels <- adm_sel$labels[!is.na(adm_sel$labels) & nzchar(adm_sel$labels)]
      } else if (length(selected_region_codes) > 0) {
        choices_df <- get_spatial_region_choices(spatial_data(), spatial_scheme_val, spatial_level_val)
        if (nrow(choices_df) > 0) {
          selected_region_labels <- choices_df$region_label[match(selected_region_codes, choices_df$region_code)]
          selected_region_labels <- selected_region_labels[!is.na(selected_region_labels) & nzchar(selected_region_labels)]
        }
      }

      new_row$spatial_level <- spatial_level_val
      new_row$spatial_region_codes <- if (length(selected_region_codes) > 0) paste(unique(selected_region_codes), collapse = "|") else NA_character_
      new_row$spatial_region_labels <- if (length(selected_region_labels) > 0) paste(unique(selected_region_labels), collapse = "|") else NA_character_
      new_row$spatial_scheme <- spatial_scheme_val
      new_row$calc_mode <- "deterministic"

      # bind_rows handles different column sets gracefully
      events_rv(dplyr::bind_rows(cur_events, new_row))
      counter(k + 1L)
    })

    # Render dynamic UI for current event only
    output$events_ui <- shiny::renderUI({
      k <- counter()

      ui_inv <- try(ui_inventory(), silent = TRUE)
      hazard_type_choices <- character(0)
      scenario_choices <- character(0)
      return_period_choices <- numeric(0)

      if (!inherits(ui_inv, "try-error") && (tibble::is_tibble(ui_inv) || is.data.frame(ui_inv)) && nrow(ui_inv) > 0) {
        hazard_type_choices <- sort(unique(ui_inv$hazard_type))
        if (length(hazard_type_choices) > 0) {
          # Get scenario values for first hazard type (primary indicator only)
          first_hazard <- hazard_type_choices[[1]]
          scenario_choices <- unique(ui_inv$scenario_name[ui_inv$hazard_type == first_hazard])

          if (length(scenario_choices) > 0) {
            # Get return periods for first hazard type and scenario (primary indicator only)
            first_scenario <- scenario_choices[[1]]
            return_period_choices <- unique(ui_inv$return_period[
              ui_inv$hazard_type == first_hazard &
                ui_inv$scenario_name == first_scenario
            ])
          }
        }
      }

      # Only show form for current event (index k) - NO hazard_indicator dropdown
      shiny::wellPanel(
        style = "padding-bottom: 0;",
        shiny::radioButtons(
          ns(paste0("calc_mode_", k)),
          label = "Mode",
          choices = c("Deterministic" = "deterministic", "Probabilistic" = "probabilistic"),
          selected = "deterministic",
          inline = TRUE
        ),
        shiny::selectInput(ns(paste0("hazard_type_", k)), "Hazard Type",
          choices = hazard_type_choices,
          selected = if (length(hazard_type_choices) > 0) hazard_type_choices[[1]] else NULL
        ),
        shiny::uiOutput(ns(paste0("dynamic_filters_ui_", k))),
        shiny::uiOutput(ns(paste0("spatial_ui_", k))),
        shiny::uiOutput(ns(paste0("year_ui_", k))),
        shiny::div(
          style = "margin: 20px -15px -10px -15px;",
          shiny::actionButton(
            ns("add_event"),
            label = "Add hazard",
            class = "btn-secondary btn-block",
            icon = shiny::icon("plus")
          )
        )
      )
    })

    # Global basin separation panel: basin level -> continental (macro) region -> basins (multi-select).
    # "Global (all basins)" in the macro dropdown simply lists every basin of the level.
    build_global_basin_ui <- function(k) {
      spatial <- spatial_data()
      if (is.null(spatial) || is.null(spatial$global_basins)) {
        return(shiny::helpText(
          "Basin boundaries are not available (expected spatial_separation/global_basins in the data folder)."
        ))
      }

      level_choices <- get_spatial_level_choices("global_basins")
      selected_level <- input[[paste0("spatial_level_", k)]]
      if (is.null(selected_level) || !selected_level %in% unname(level_choices)) {
        selected_level <- GLOBAL_WHOLE_AREA_LEVEL
      }

      macro_input <- NULL
      region_input <- NULL
      if (!selected_level %in% whole_area_spatial_levels()) {
        macros <- get_global_basin_macro_regions(spatial, selected_level)
        macro_choices <- c("Global (all basins)" = "__all__", stats::setNames(macros, macros))
        selected_macro <- input[[paste0("spatial_macro_", k)]]
        if (is.null(selected_macro) || !selected_macro %in% unname(macro_choices)) {
          selected_macro <- "__all__"
        }
        macro_input <- shiny::selectInput(
          ns(paste0("spatial_macro_", k)),
          label = "Continental region",
          choices = macro_choices,
          selected = selected_macro
        )

        all_level <- get_global_basin_choices(spatial, selected_level)
        in_macro <- get_global_basin_choices(spatial, selected_level, selected_macro)
        current_sel <- input[[paste0("spatial_regions_", k)]]
        if (is.null(current_sel)) current_sel <- character(0)
        current_sel <- intersect(as.character(current_sel), all_level$region_code)
        # Keep already-selected basins selectable when the macro region changes.
        shown <- dplyr::bind_rows(
          in_macro,
          all_level[all_level$region_code %in% current_sel, , drop = FALSE]
        ) |>
          dplyr::distinct(.data$region_code, .keep_all = TRUE) |>
          dplyr::arrange(.data$region_label)
        region_choices <- stats::setNames(as.character(shown$region_code), as.character(shown$region_label))

        region_input <- shiny::selectizeInput(
          ns(paste0("spatial_regions_", k)),
          label = "Selected Basins",
          choices = region_choices,
          selected = current_sel,
          multiple = TRUE,
          options = list(placeholder = "Choose one or more basins (none = whole area)")
        )
      }

      shiny::wellPanel(
        style = "margin-top: 8px; margin-bottom: 8px; padding: 10px;",
        shiny::h5("Spatial Separation", style = "margin-top: 0;"),
        shiny::selectInput(
          ns(paste0("spatial_level_", k)),
          label = "Basin Level",
          choices = level_choices,
          selected = selected_level
        ),
        macro_input,
        region_input
      )
    }

    # Global administrative separation panel (Heat, Drought): level -> countries -> regions, multi-select.
    # ADM0 stops at the countries; ADM1 / ADM2 then list the regions of the chosen countries
    # (the ADM2 list has 49k entries, so it is only built for explicit countries).
    build_global_adm_ui <- function(k) {
      spatial <- spatial_data()
      if (is.null(spatial) || is.null(spatial$global_adm)) {
        return(shiny::helpText(
          "Administrative boundaries are not available (expected spatial_separation/global_adm_regions in the data folder)."
        ))
      }

      level_choices <- get_spatial_level_choices("global_adm")
      selected_level <- input[[paste0("spatial_level_", k)]]
      if (is.null(selected_level) || !selected_level %in% unname(level_choices)) {
        selected_level <- GLOBAL_WHOLE_AREA_LEVEL
      }

      country_input <- NULL
      region_input <- NULL
      if (!selected_level %in% whole_area_spatial_levels()) {
        countries <- get_global_adm_country_choices(spatial)
        country_choices <- stats::setNames(as.character(countries$region_code), as.character(countries$region_label))
        current_countries <- input[[paste0("spatial_countries_", k)]]
        if (is.null(current_countries)) current_countries <- character(0)
        current_countries <- intersect(as.character(current_countries), unname(country_choices))

        is_country_level <- identical(selected_level, "adm0")
        country_input <- shiny::selectizeInput(
          ns(paste0("spatial_countries_", k)),
          label = if (is_country_level) "Selected Countries" else "Country",
          choices = country_choices,
          selected = current_countries,
          multiple = TRUE,
          options = list(
            placeholder = if (is_country_level) {
              "Choose one or more countries (none = whole area)"
            } else {
              "Choose one or more countries"
            }
          )
        )

        if (!is_country_level) {
          if (length(current_countries) == 0) {
            region_input <- shiny::helpText("Choose a country to list its regions.")
          } else {
            regions <- get_global_adm_region_choices(spatial, selected_level, current_countries)
            region_choices <- stats::setNames(as.character(regions$region_code), as.character(regions$region_label))
            current_sel <- input[[paste0("spatial_regions_", k)]]
            if (is.null(current_sel)) current_sel <- character(0)
            current_sel <- intersect(as.character(current_sel), unname(region_choices))
            region_input <- shiny::selectizeInput(
              ns(paste0("spatial_regions_", k)),
              label = "Selected Regions",
              choices = region_choices,
              selected = current_sel,
              multiple = TRUE,
              options = list(placeholder = "Choose one or more regions (none = whole countries)")
            )
          }
        }
      }

      shiny::wellPanel(
        style = "margin-top: 8px; margin-bottom: 8px; padding: 10px;",
        shiny::h5("Spatial Separation", style = "margin-top: 0;"),
        shiny::selectInput(
          ns(paste0("spatial_level_", k)),
          label = "Granularity",
          choices = level_choices,
          selected = selected_level
        ),
        country_input,
        region_input
      )
    }

    # Create cascading dropdowns based on index configuration
    shiny::observe({
      k <- counter()
      if (k == 0) return()

      output[[paste0("dynamic_filters_ui_", k)]] <- shiny::renderUI({
        ui_inv <- try(ui_inventory(), silent = TRUE)
        hazard_type_val <- input[[paste0("hazard_type_", k)]]
        
        if (inherits(ui_inv, "try-error") || is.null(ui_inv) || nrow(ui_inv) == 0 || is.null(hazard_type_val)) {
          return(shiny::span(""))
        }

        # Get index configuration for this hazard type
        cfg <- try(hazard_configs(), silent = TRUE)
        index_ind <- get_index_indicator(cfg, hazard_type_val)
        index_cols <- cfg[[hazard_type_val]]$indicators[[index_ind]]$index

        # Probabilistic mode uses every available return period at once (see the
        # AAL engine), so the return_period filter is not shown -- only scenario.
        calc_mode_val <- input[[paste0("calc_mode_", k)]]
        if (identical(calc_mode_val, "probabilistic")) {
          index_cols <- setdiff(index_cols, "return_period")
          # Global drought AAL integrates all four seasons, so no season is chosen.
          if (!inherits(cfg, "try-error") && hazard_type_val %in% global_drought_hazard_types(cfg)) {
            index_cols <- setdiff(index_cols, "season")
          }
        }

        if (length(index_cols) == 0) return(shiny::helpText("No index defined for this hazard"))

        # Create a list of selectInputs, one for each index column
        # They cascade: each one filters the available choices for the next
        filter_uis <- list()
        temp_inv <- ui_inv |> dplyr::filter(.data$hazard_type == hazard_type_val)
        
        for (i in seq_along(index_cols)) {
          idx_col <- index_cols[i]
          
          # Label is the index column name itself (dynamic!)
          label <- idx_col
          # Prettify common names if desired, but keep it dynamic
          if (label == "scenario_name") label <- "Scenario"
          if (label == "return_period") label <- "Return Period (years)"
          if (label == "gwl") label <- "GWL"
          if (label == "season") label <- "Season"
          if (label == "statistic") label <- "Statistic"

          choices <- sort(unique(temp_inv[[idx_col]]))
          # Heat / drought statistic: median first so it is the default selection
          if (idx_col == "statistic") choices <- order_heat_statistics(choices)
          # Seasons in calendar order (DJF, MAM, JJA, SON); other labels keep sorted order
          if (idx_col == "season") choices <- order_drought_seasons(choices)
          
          # Check if we already have a selection for this input
          input_id <- paste0("filter_", k, "_", idx_col)
          current_selection <- input[[input_id]]
          
          if (is.null(current_selection) || !(current_selection %in% choices)) {
            current_selection <- if (length(choices) > 0) choices[1] else NULL
          }

          filter_uis[[i]] <- shiny::selectInput(ns(input_id), label,
            choices = choices,
            selected = current_selection
          )
          
          # Filter the inventory for the next iteration based on this selection
          if (!is.null(current_selection)) {
             # Match types for filtering
             val_to_filter <- current_selection
             if (idx_col == "return_period") val_to_filter <- as.numeric(val_to_filter)
             temp_inv <- temp_inv |> dplyr::filter(.data[[idx_col]] == !!val_to_filter)
          }
        }
        
        shiny::tagList(filter_uis)
      })

      output[[paste0("spatial_ui_", k)]] <- shiny::renderUI({
        hazard_type_val <- input[[paste0("hazard_type_", k)]]
        if (is.null(hazard_type_val) || !nzchar(as.character(hazard_type_val))) {
          return(shiny::span(""))
        }

        region_val <- if (!is.null(region_reactive)) region_reactive() else "brazil"
        if (identical(region_val, "global")) {
          # Global mode: basin separation applies to deterministic Flood, administrative-region
          # separation to deterministic Heat and Drought.
          is_prob <- identical(input[[paste0("calc_mode_", k)]], "probabilistic")
          global_cfg <- try(hazard_configs(), silent = TRUE)
          is_heat <- !inherits(global_cfg, "try-error") && !is.null(global_cfg) &&
            as.character(hazard_type_val) %in% global_adm_hazard_types(global_cfg)
          if (is_prob || !(identical(as.character(hazard_type_val), "Flood") || is_heat)) {
            return(shiny::p(
              class = "text-muted",
              style = "margin-top: 8px; font-size: 0.85em;",
              "Spatial separation is not available for this hazard/mode in Global mode - analysis covers the whole selected area."
            ))
          }
          return(if (is_heat) build_global_adm_ui(k) else build_global_basin_ui(k))
        }

        calc_mode_val <- input[[paste0("calc_mode_", k)]]
        if (identical(calc_mode_val, "probabilistic")) {
          return(shiny::p(
            class = "text-muted",
            style = "margin-top: 8px; font-size: 0.85em;",
            "Spatial separation is not available in Probabilistic mode — the Average Annual Loss covers the whole selected area."
          ))
        }

        cfg <- try(hazard_configs(), silent = TRUE)
        if (inherits(cfg, "try-error") || is.null(cfg) || !hazard_type_val %in% names(cfg)) {
          return(shiny::span(""))
        }

        scheme <- get_hazard_spatial_scheme(cfg, hazard_type_val)
        level_choices <- get_spatial_level_choices(scheme)
        selected_level <- input[[paste0("spatial_level_", k)]]
        if (is.null(selected_level) || !selected_level %in% unname(level_choices)) {
          selected_level <- "brazil"
        }

        region_input <- NULL
        if (selected_level != "brazil") {
          choices_df <- get_spatial_region_choices(spatial_data(), scheme, selected_level)
          if (nrow(choices_df) > 0) {
            region_choices <- as.character(choices_df$region_code)
            names(region_choices) <- as.character(choices_df$region_label)

            current_sel <- input[[paste0("spatial_regions_", k)]]
            if (is.null(current_sel)) current_sel <- character(0)
            current_sel <- intersect(as.character(current_sel), unname(region_choices))

            region_input <- shiny::selectizeInput(
              ns(paste0("spatial_regions_", k)),
              label = "Selected Regions",
              choices = region_choices,
              selected = current_sel,
              multiple = TRUE,
              options = list(placeholder = "Choose one or more regions")
            )
          } else {
            region_input <- shiny::helpText("No spatial regions available for this level.")
          }
        }

        shiny::wellPanel(
          style = "margin-top: 8px; margin-bottom: 8px; padding: 10px;",
          shiny::h5("Spatial Separation", style = "margin-top: 0;"),
          shiny::selectInput(
            ns(paste0("spatial_level_", k)),
            label = "Separation Level",
            choices = level_choices,
            selected = selected_level
          ),
          region_input
        )
      })

      output[[paste0("year_ui_", k)]] <- shiny::renderUI({
        calc_mode_val <- input[[paste0("calc_mode_", k)]]
        if (identical(calc_mode_val, "probabilistic")) {
          return(shiny::p(
            class = "text-muted",
            style = "margin-top: 8px; font-size: 0.85em;",
            "Shock year is not applicable in Probabilistic mode — the Average Annual Loss is deducted from every year of the projection."
          ))
        }

        shiny::sliderInput(
          ns(paste0("year_", k)),
          label = "Shock Year:",
          value = 2030,
          min = 2025,
          max = 2049,
          step = 1,
          sep = "",
          ticks = TRUE
        )
      })
    })


    output$download_config <- shiny::downloadHandler(
      filename = function() {
        paste0("hazard_configuration_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        current_events <- events_rv()

        if (is.null(current_events) || nrow(current_events) == 0) {
          writexl::write_xlsx(
            tibble::tibble(message = "No hazard configuration available"),
            path = file
          )
          return()
        }

        # Persist only index-defining columns; hazard metadata is derived on load.
        # NOTE: calc_mode is exported for reference, but Load Events does not yet
        # reconstruct probabilistic (AAL) rows from an uploaded file -- only
        # deterministic rows round-trip through Save/Load in this version.
        export_cols <- c(
          "event_id",
          "hazard_type",
          "scenario_name",
          "return_period",
          "event_year",
          "season",
          "statistic",
          "spatial_level",
          "spatial_region_codes",
          "spatial_region_labels",
          "spatial_scheme",
          "calc_mode"
        )

        export_df <- current_events |>
          dplyr::select(dplyr::any_of(export_cols))

        writexl::write_xlsx(as.data.frame(export_df), path = file)
      }
    )

    # Delete event function
    delete_event <- function(event_id) {
      updated <- delete_event_by_id(events_rv(), event_id)
      events_rv(updated)
    }

    # Handle file upload from the hidden file input
    shiny::observeEvent(input$upload_hazard_config, {
      upload <- input$upload_hazard_config
      if (!is.null(upload) && !is.null(upload$datapath) && file.exists(upload$datapath)) {
        load_config(upload$datapath)
      }
    })

    # Load config from external file path
    load_config <- function(file_path) {
      cfg <- try(hazard_configs(), silent = TRUE)
      if (inherits(cfg, "try-error") || is.null(cfg)) {
        return()
      }

      inv <- try(hazards_inventory(), silent = TRUE)
      if (inherits(inv, "try-error")) {
        inv <- NULL
      }

      reconstructed <- load_hazards_events_config(
        file_path = file_path,
        hazard_configs = cfg,
        hazards_inventory = inv
      )

      if (is.null(reconstructed) || nrow(reconstructed) == 0) {
        return()
      }

      events_rv(reconstructed)
      counter(nrow(reconstructed) + 1L)
    }

    # Return
    return(list(
      events = events_rv,
      delete_event = delete_event,
      load_config = load_config
    ))
  })
}
