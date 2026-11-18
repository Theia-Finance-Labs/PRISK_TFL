#' trisk_prisk_control UI Function
#'
#' @description Sidebar control module for the TRISK-PRISK Integration mode:
#'   PRISK asset/company folder picker, TRISK output file selector, hazard
#'   selection (Flood only, kept for UX consistency with the Physical Risk
#'   Analysis mode's Add Hazard flow), its own Financial Parameters sliders
#'   (independent of, and styled identically to, the Physical Risk Analysis
#'   mode's), a terminal value toggle, and the run button.
#' @param id Internal parameter for shiny
#' @export
mod_trisk_prisk_control_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "control-section",
      shiny::h4("PRISK Asset/Company Inputs", class = "section-header"),
      shiny::p(
        "Folder containing the PRISK-schema assets/financials/portfolio files (identical schema to Physical Risk Analysis mode).",
        class = "text-muted", style = "font-size: 0.85em; margin-bottom: 10px;"
      ),
      shinyFiles::shinyDirButton(
        id = ns("select_prisk_folder"),
        label = "Select PRISK Input Folder",
        title = "Select folder containing assets.xlsx/.csv, financials.xlsx/.csv, portfolio.xlsx/.csv",
        icon = shiny::icon("folder-open"),
        class = "btn-primary btn-block"
      ),
      shiny::div(
        style = "margin-top: 10px; padding: 8px; border-radius: 4px; font-size: 0.9em;",
        shiny::textOutput(ns("prisk_folder_status"))
      )
    ),
    shiny::div(
      class = "control-section",
      shiny::h4("TRISK Output File", class = "section-header"),
      shiny::p(
        "The asset-level transition-risk profit pathway file (company_trajectories.csv or .xlsx).",
        class = "text-muted", style = "font-size: 0.85em; margin-bottom: 10px;"
      ),
      shiny::fileInput(
        ns("trisk_file"),
        label = NULL,
        accept = c(".csv", ".xlsx"),
        buttonLabel = "Browse...",
        placeholder = "company_trajectories.csv"
      ),
      shiny::uiOutput(ns("trisk_file_status"))
    ),
    shiny::div(
      class = "control-section",
      shiny::h4("Hazard", class = "section-header"),
      shiny::p(
        "Only flood physical risk is supported in this mode; select and add it to proceed.",
        class = "text-muted", style = "font-size: 0.85em; margin-bottom: 10px;"
      ),
      shiny::selectInput(
        ns("hazard_type"),
        "Hazard Type",
        choices = c("Flood" = "Flood"),
        selected = "Flood"
      ),
      shiny::actionButton(
        ns("add_hazard"),
        "Add Hazard",
        class = "btn-secondary btn-block",
        icon = shiny::icon("plus")
      ),
      shiny::uiOutput(ns("hazard_status"))
    ),
    shiny::div(
      class = "control-section",
      shiny::h4("Financial Parameters", class = "section-header"),
      shiny::p("Adjust these rates to reflect your financial assumptions:", class = "text-muted", style = "font-size: 0.9em; margin-bottom: 10px;"),
      shiny::sliderInput(
        ns("growth_rate"),
        "Revenue Growth (%):",
        value = 2.0,
        min = 0,
        max = 10,
        step = 0.1,
        ticks = FALSE
      ),
      shiny::sliderInput(
        ns("discount_rate"),
        "Discount Rate (%):",
        value = 5.0,
        min = 0,
        max = 15,
        step = 0.1,
        ticks = FALSE
      ),
      shiny::sliderInput(
        ns("risk_free_rate"),
        "Risk-Free Rate (%):",
        value = 2.0,
        min = 0,
        max = 10,
        step = 0.1,
        ticks = FALSE
      )
    ),
    shiny::div(
      class = "control-section",
      shiny::h4("Terminal Value", class = "section-header"),
      shiny::checkboxInput(
        ns("include_terminal_value"),
        "Include terminal value (growing perpetuity beyond 2050)",
        value = TRUE
      )
    ),
    shiny::div(
      class = "control-section run-section",
      shiny::actionButton(
        ns("run_trisk_prisk"),
        "Run TRISK-PRISK Analysis",
        class = "btn-primary btn-lg btn-block",
        icon = shiny::icon("play")
      ),
      shiny::div(
        style = "margin-top: 10px; padding: 8px; border-radius: 4px; font-size: 0.9em;",
        shiny::textOutput(ns("run_status"))
      )
    )
  )
}

#' trisk_prisk_control Server Functions
#'
#' @param id Internal parameter for shiny
#' @param base_dir_reactive reactive containing base directory path (for the folder picker's default volume)
#' @param status_reactive optional reactive returning the current run-status message to display
#' @return list with reactives: prisk_folder, trisk_file_path, hazard_added,
#'   include_terminal_value, growth_rate, discount_rate, risk_free_rate, run_trigger
#' @export
mod_trisk_prisk_control_server <- function(
  id,
  base_dir_reactive,
  status_reactive = NULL
) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    values <- shiny::reactiveValues(selected_prisk_folder = NULL)
    hazard_added <- shiny::reactiveVal(FALSE)

    volumes <- shiny::reactive({
      base_dir <- base_dir_reactive()
      vol_list <- c(Home = path.expand("~"), shinyFiles::getVolumes()())
      if (!is.null(base_dir) && base_dir != "" && dir.exists(base_dir)) {
        vol_list <- c("Base Directory" = base_dir, vol_list)
      }
      return(vol_list)
    })

    shiny::observe({
      vol_list <- volumes()
      shinyFiles::shinyDirChoose(
        input = input,
        id = "select_prisk_folder",
        roots = vol_list,
        session = session,
        filetypes = character(0),
        allowDirCreate = FALSE
      )
    })

    shiny::observeEvent(input$select_prisk_folder, {
      if (!is.integer(input$select_prisk_folder)) {
        folder_path <- shinyFiles::parseDirPath(volumes(), input$select_prisk_folder)
        if (length(folder_path) > 0 && dir.exists(folder_path)) {
          values$selected_prisk_folder <- as.character(folder_path)
        }
      }
    })

    output$prisk_folder_status <- shiny::renderText({
      if (is.null(values$selected_prisk_folder)) {
        return("No folder selected")
      }
      file_status <- resolve_input_file_status(values$selected_prisk_folder)
      asset_ok <- file_status$asset$status %in% c("legacy", "new")
      company_ok <- file_status$company$status %in% c("legacy", "new")
      status <- if (asset_ok && company_ok) "[OK] " else "[X] "
      paste0(status, "Folder: ", values$selected_prisk_folder)
    })

    output$trisk_file_status <- shiny::renderUI({
      if (is.null(input$trisk_file)) {
        return(shiny::p("No file selected", class = "text-muted", style = "font-size: 0.9em;"))
      }
      shiny::p(
        paste0("[OK] ", input$trisk_file$name),
        class = "text-muted", style = "font-size: 0.9em;"
      )
    })

    # Reset the "hazard added" state whenever the hazard type selection changes,
    # mirroring the Physical Risk Analysis mode's per-event Add Hazard flow.
    shiny::observeEvent(input$hazard_type, {
      hazard_added(FALSE)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$add_hazard, {
      if (!is.null(input$hazard_type) && nzchar(input$hazard_type)) {
        hazard_added(TRUE)
      }
    })

    output$hazard_status <- shiny::renderUI({
      if (isTRUE(hazard_added())) {
        shiny::p(
          paste0("[OK] ", input$hazard_type, " added."),
          class = "text-muted", style = "font-size: 0.9em;"
        )
      } else {
        shiny::p(
          "No hazard added yet.",
          class = "text-muted", style = "font-size: 0.9em;"
        )
      }
    })

    output$run_status <- shiny::renderText({
      if (is.null(status_reactive)) return("")
      status <- try(status_reactive(), silent = TRUE)
      if (inherits(status, "try-error") || is.null(status)) return("")
      status
    })

    list(
      prisk_folder = shiny::reactive({ values$selected_prisk_folder }),
      trisk_file_path = shiny::reactive({
        if (is.null(input$trisk_file)) return(NULL)
        list(datapath = input$trisk_file$datapath, name = input$trisk_file$name)
      }),
      hazard_added = shiny::reactive({ isTRUE(hazard_added()) }),
      include_terminal_value = shiny::reactive({ isTRUE(input$include_terminal_value) }),
      growth_rate = shiny::reactive({ input$growth_rate / 100 }),
      discount_rate = shiny::reactive({ input$discount_rate / 100 }),
      risk_free_rate = shiny::reactive({ input$risk_free_rate / 100 }),
      run_trigger = shiny::reactive({ input$run_trisk_prisk })
    )
  })
}
