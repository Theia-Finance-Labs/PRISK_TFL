#' trisk_prisk_pathways UI Function
#'
#' @description Module to display side-by-side TRISK-only vs. TRISK+PRISK
#'   company-level profit pathways for a selected company.
#' @param id Internal parameter for shiny
#' @export
mod_trisk_prisk_pathways_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "results-container",
      shiny::h3("Profit Pathways: TRISK-only vs. TRISK+PRISK", class = "results-title"),
      shiny::p(
        "Select a company to compare its transition-risk-only profit pathway against the physical-risk-adjusted pathway.",
        class = "text-muted", style = "margin-bottom: 1rem;"
      ),
      shiny::div(
        style = "max-width: 500px; margin-bottom: 0.5rem;",
        shiny::uiOutput(ns("company_select_ui"))
      ),
      shiny::uiOutput(ns("company_meta_display")),
      shiny::div(
        style = "display: flex; gap: 1.5rem; flex-wrap: wrap; margin-top: 1.5rem;",
        shiny::div(
          class = "chart-container",
          style = "flex: 1 1 420px; min-width: 320px;",
          shiny::h4("TRISK-only", class = "section-header"),
          plotly::plotlyOutput(ns("trisk_only_plot"), height = "420px")
        ),
        shiny::div(
          class = "chart-container",
          style = "flex: 1 1 420px; min-width: 320px;",
          shiny::h4("TRISK+PRISK", class = "section-header"),
          plotly::plotlyOutput(ns("trisk_prisk_plot"), height = "420px")
        )
      )
    )
  )
}

#' trisk_prisk_pathways Server Functions
#'
#' @param id Internal parameter for shiny
#' @param results_reactive reactive returning the compute_risk_trisk_prisk() result list (or NULL)
#' @param trisk_joined_reactive reactive returning the joined TRISK-PRISK asset table (for sector/technology/country_iso2 display)
#' @export
mod_trisk_prisk_pathways_server <- function(id, results_reactive, trisk_joined_reactive = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    has_results <- shiny::reactive({
      results <- results_reactive()
      !is.null(results) && !is.null(results$company_yearly) && nrow(results$company_yearly) > 0
    })

    output$company_select_ui <- shiny::renderUI({
      if (!has_results()) {
        return(shiny::p("Run the analysis to select a company.", class = "text-muted"))
      }
      companies <- sort(unique(results_reactive()$company_yearly$company))
      shiny::selectizeInput(
        ns("selected_company"),
        label = "Company",
        choices = companies,
        selected = companies[[1]],
        multiple = FALSE
      )
    })

    company_metadata <- shiny::reactive({
      if (is.null(trisk_joined_reactive)) return(NULL)
      trisk_df <- try(trisk_joined_reactive(), silent = TRUE)
      if (inherits(trisk_df, "try-error") || is.null(trisk_df) || nrow(trisk_df) == 0) return(NULL)
      trisk_df |>
        dplyr::select(dplyr::any_of(c("company", "sector", "technology", "country_iso2"))) |>
        dplyr::distinct()
    })

    output$company_meta_display <- shiny::renderUI({
      req_company <- input$selected_company
      meta <- company_metadata()
      if (is.null(req_company) || is.null(meta)) return(NULL)

      row <- meta[meta$company == req_company, ]
      if (nrow(row) == 0) return(NULL)
      row <- row[1, ]

      parts <- c()
      if ("sector" %in% names(row) && !is.na(row$sector)) parts <- c(parts, paste0("Sector: ", row$sector))
      if ("technology" %in% names(row) && !is.na(row$technology) && nzchar(as.character(row$technology))) {
        parts <- c(parts, paste0("Technology: ", row$technology))
      }
      if ("country_iso2" %in% names(row) && !is.na(row$country_iso2)) parts <- c(parts, paste0("Country: ", row$country_iso2))
      if (length(parts) == 0) return(NULL)

      shiny::p(paste(parts, collapse = "  |  "), class = "text-muted", style = "font-size: 0.9em;")
    })

    shared_y_range <- shiny::reactive({
      if (!has_results() || is.null(input$selected_company)) return(NULL)
      cy <- results_reactive()$company_yearly |>
        dplyr::filter(.data$company == input$selected_company)
      if (nrow(cy) == 0) return(NULL)
      vals <- cy$total_profit[is.finite(cy$total_profit)]
      if (length(vals) == 0) return(NULL)
      pad <- diff(range(vals)) * 0.08
      if (!is.finite(pad) || pad == 0) pad <- max(abs(vals), 1) * 0.1
      c(min(vals) - pad, max(vals) + pad)
    })

    render_panel <- function(scenario_baseline, scenario_shock) {
      plotly::renderPlotly({
        if (!has_results() || is.null(input$selected_company)) {
          return(
            plotly::plot_ly() |>
              plotly::add_text(
                x = 0.5, y = 0.5, text = "Run the analysis and select a company",
                textposition = "middle center", showlegend = FALSE
              ) |>
              plotly::layout(
                xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
                yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
              )
          )
        }

        cy <- results_reactive()$company_yearly |>
          dplyr::filter(.data$company == input$selected_company)

        baseline_df <- cy |> dplyr::filter(.data$scenario == scenario_baseline) |> dplyr::arrange(.data$year)
        shock_df <- cy |> dplyr::filter(.data$scenario == scenario_shock) |> dplyr::arrange(.data$year)

        y_range <- shared_y_range()

        plotly::plot_ly() |>
          plotly::add_trace(
            data = baseline_df, x = ~year, y = ~total_profit, type = "scatter", mode = "lines+markers",
            name = "Baseline", line = list(color = "#009C3B", width = 2.5), marker = list(color = "#009C3B", size = 5)
          ) |>
          plotly::add_trace(
            data = shock_df, x = ~year, y = ~total_profit, type = "scatter", mode = "lines+markers",
            name = "Shock", line = list(color = "#C21807", width = 2.5), marker = list(color = "#C21807", size = 5)
          ) |>
          plotly::layout(
            xaxis = list(title = "Year", dtick = 5),
            yaxis = list(title = "Company Profit ($)", range = y_range, tickformat = ",.0f"),
            legend = list(orientation = "h", x = 0, y = 1.12),
            margin = list(t = 20)
          )
      })
    }

    output$trisk_only_plot <- render_panel("trisk_baseline", "trisk_shock")
    output$trisk_prisk_plot <- render_panel("trisk_prisk_baseline", "trisk_prisk_shock")
  })
}
