#' The application User-Interface
#'
#' @param request Internal parameter for `shiny`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      class = "climate-risk-app",

      # Header
      div(
        class = "app-header",
        div(
          class = "app-header-text",
          h1("Physical Risk Analysis Tool", class = "app-title"),
          p("Physical risk assessment for financial portfolios", class = "app-subtitle")
        ),
        div(
          class = "app-header-actions",
          div(
            class = "app-mode-toggle",
            radioButtons(
              inputId = "app_mode",
              label = "Mode:",
              choices = c("Physical Risk Analysis" = "prisk", "TRISK-PRISK Integration" = "trisk_prisk"),
              selected = "prisk",
              inline = TRUE
            )
          ),
          conditionalPanel(
            condition = "input.app_mode == 'prisk'",
            div(
              class = "app-region-toggle",
              radioButtons(
                inputId = "region",
                label = "Region:",
                choices = c("Brazil" = "brazil", "Global" = "global"),
                selected = "brazil",
                inline = TRUE
              )
            )
          ),
          actionButton(
            inputId = "open_settings",
            label = "Settings",
            icon = icon("gear"),
            class = "btn btn-secondary app-settings-btn"
          )
        )
      ),
      # Physical Risk Analysis mode: Region-driven single-hazard flood tool
      # (Brazil/Global deterministic and probabilistic flood analysis).
      # Hidden (not unmounted) when app_mode != "prisk", so its inputs --
      # including the Financial Parameters growth/discount/risk-free rate
      # sliders that TRISK-PRISK mode reuses directly -- keep their values.
      conditionalPanel(
        condition = "input.app_mode == 'prisk'",
        div(
          class = "app-body",
          sidebarLayout(
            # Sidebar with controls
            sidebarPanel(
              class = "app-sidebar",
              width = 3,
              mod_control_ui("control")
            ),

            # Main content with tabs
            mainPanel(
              class = "app-main",
              width = 9,
              tabsetPanel(
                id = "main_tabs",
                type = "pills",
                selected = "status",

                # Tab 1: Parameters and Status
                tabPanel(
                  title = "Parameters & Status",
                  value = "status",
                  icon = icon("cog"),
                  mod_status_ui("status")
                ),

                # Tab 2: Asset Results (shown only after results)
                tabPanel(
                  title = "Asset Analysis",
                  value = "assets",
                  icon = icon("table"),
                  mod_results_assets_ui("results_assets")
                ),

                # Tab 3: Asset Profit Pathways
                tabPanel(
                  title = "Profit Pathways",
                  value = "pathways",
                  icon = icon("chart-line"),
                  mod_profit_pathways_ui("profit_pathways")
                ),

                # Tab 4: Company Analysis
                tabPanel(
                  title = "Company Analysis",
                  value = "companies",
                  icon = icon("building"),
                  mod_company_analysis_ui("company_analysis")
                )
              )
            )
          )
        )
      ),
      # TRISK-PRISK Integration mode: a separate top-level flow -- its own
      # sidebar (PRISK asset/company folder + TRISK output file + terminal
      # value toggle) and its own tabs (Profit Pathways / Outputs comparison).
      # Reuses the Physical Risk Analysis mode's Financial Parameters rates
      # directly (see mod_trisk_prisk_control.R) rather than duplicating them.
      conditionalPanel(
        condition = "input.app_mode == 'trisk_prisk'",
        div(
          class = "app-body",
          sidebarLayout(
            sidebarPanel(
              class = "app-sidebar",
              width = 3,
              mod_trisk_prisk_control_ui("trisk_prisk_control")
            ),
            mainPanel(
              class = "app-main",
              width = 9,
              tabsetPanel(
                id = "trisk_prisk_tabs",
                type = "pills",
                selected = "trisk_prisk_asset_analysis",

                tabPanel(
                  title = "PRISK Analysis",
                  value = "trisk_prisk_asset_analysis",
                  icon = icon("water"),
                  mod_trisk_prisk_asset_analysis_ui("trisk_prisk_asset_analysis")
                ),
                tabPanel(
                  title = "Profit Pathways",
                  value = "trisk_prisk_pathways",
                  icon = icon("chart-line"),
                  mod_trisk_prisk_pathways_ui("trisk_prisk_pathways")
                ),
                tabPanel(
                  title = "Outputs",
                  value = "trisk_prisk_outputs",
                  icon = icon("table"),
                  mod_trisk_prisk_outputs_ui("trisk_prisk_outputs")
                )
              )
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "climate.risk.tool"
    ),
    # Add custom CSS
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "www/custom.css"
    ),
    tags$script(src = "www/status-repro.js"),
    # Add Font Awesome for icons
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"
    )
  )
}
