#' trisk_prisk_outputs UI Function
#'
#' @description Module to display the recomputed NPV/PD/Expected Loss comparison
#'   across all four TRISK-PRISK scenarios: a portfolio-level grouped bar chart
#'   (Expected Loss, Baseline vs. Shock, grouped by TRISK-only vs. TRISK+PRISK,
#'   summed across every company), the same chart for a single selected company,
#'   and a full NPV/PD/Expected Loss comparison table across every company.
#' @param id Internal parameter for shiny
#' @export
mod_trisk_prisk_outputs_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "results-container",
      shiny::h3("TRISK-PRISK Outputs Comparison", class = "results-title"),
      shiny::p(
        "Recomputed NPV, Probability of Default, and Expected Loss across all four scenarios: TRISK-only baseline/shock and TRISK+PRISK baseline/shock.",
        class = "text-muted", style = "margin-bottom: 1rem;"
      ),
      shiny::div(
        style = "margin-bottom: 2rem;",
        shiny::h4("Total Portfolio Overview", class = "section-header"),
        shiny::p(
          "NPV and Expected Loss are summed across every company in the portfolio; Probability of Default is a simple average across companies (PDs are not additive).",
          class = "text-muted", style = "font-size: 0.9em; margin-bottom: 0.5rem;"
        ),
        shiny::div(
          class = "events-table-container",
          DT::dataTableOutput(ns("portfolio_summary_table"))
        )
      ),
      shiny::div(
        class = "chart-container",
        style = "margin-bottom: 2rem;",
        shiny::h4("Portfolio Expected Loss: Baseline vs. Shock, TRISK-only vs. TRISK+PRISK", class = "section-header"),
        plotly::plotlyOutput(ns("portfolio_expected_loss_plot"), height = "420px")
      ),
      shiny::div(
        style = "max-width: 500px; margin-bottom: 1rem;",
        shiny::uiOutput(ns("company_select_ui"))
      ),
      shiny::div(
        class = "chart-container",
        shiny::h4("Expected Loss: Baseline vs. Shock, TRISK-only vs. TRISK+PRISK", class = "section-header"),
        plotly::plotlyOutput(ns("expected_loss_grouped_plot"), height = "420px")
      ),
      shiny::div(
        style = "margin-top: 2rem;",
        shiny::h4("All Companies: NPV / PD / Expected Loss by Scenario", class = "section-header"),
        shiny::div(
          class = "results-downloads",
          shiny::downloadButton(ns("download_results_csv"), "Download Results (CSV)", class = "btn btn-info")
        ),
        shiny::div(
          class = "events-table-container",
          style = "margin-top: 0.5rem;",
          DT::dataTableOutput(ns("results_table"))
        )
      )
    )
  )
}

#' trisk_prisk_outputs Server Functions
#'
#' @param id Internal parameter for shiny
#' @param results_reactive reactive returning the compute_risk_trisk_prisk() result list (or NULL)
#' @export
mod_trisk_prisk_outputs_server <- function(id, results_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    has_results <- shiny::reactive({
      results <- results_reactive()
      !is.null(results) && !is.null(results$companies) && nrow(results$companies) > 0
    })

    portfolio_summary_raw <- shiny::reactive({
      if (!has_results()) return(NULL)
      df <- results_reactive()$companies

      scenario_defs <- tibble::tibble(
        key = c("trisk_baseline", "trisk_shock", "trisk_prisk_baseline", "trisk_prisk_shock"),
        label = c("TRISK-only Baseline", "TRISK-only Shock", "TRISK+PRISK Baseline", "TRISK+PRISK Shock"),
        group = c("TRISK-only", "TRISK-only", "TRISK+PRISK", "TRISK+PRISK"),
        pathway = c("Baseline", "Shock", "Baseline", "Shock")
      )

      purrr::pmap_dfr(scenario_defs, function(key, label, group, pathway) {
        tibble::tibble(
          group = group,
          pathway = pathway,
          "Scenario" = label,
          total_npv = sum(df[[paste0("npv_", key)]], na.rm = TRUE),
          avg_pd = mean(df[[paste0("pd_", key)]], na.rm = TRUE),
          total_el = sum(df[[paste0("expected_loss_", key)]], na.rm = TRUE),
          n_companies = sum(!is.na(df[[paste0("npv_", key)]]))
        )
      })
    })

    portfolio_summary_data <- shiny::reactive({
      base <- portfolio_summary_raw()
      if (is.null(base)) return(NULL)

      fmt_curr <- function(x) paste0("$", formatC(round(x), format = "f", digits = 0, big.mark = ","))

      base |>
        dplyr::transmute(
          "Scenario" = .data$Scenario,
          "Total NPV" = fmt_curr(.data$total_npv),
          "Average PD (%)" = round(.data$avg_pd * 100, 3),
          "Total Expected Loss" = fmt_curr(.data$total_el),
          "Companies" = .data$n_companies
        )
    })

    output$portfolio_expected_loss_plot <- plotly::renderPlotly({
      base <- portfolio_summary_raw()
      if (is.null(base)) {
        return(
          plotly::plot_ly() |>
            plotly::add_text(
              x = 0.5, y = 0.5, text = "Run the analysis to see portfolio Expected Loss",
              textposition = "middle center", showlegend = FALSE
            ) |>
            plotly::layout(
              xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
              yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
            )
        )
      }

      plotly::plot_ly(
        data = base, x = ~pathway, y = ~total_el, color = ~group,
        colors = c("TRISK-only" = "#002776", "TRISK+PRISK" = "#FF6B35"),
        type = "bar"
      ) |>
        plotly::layout(
          barmode = "group",
          xaxis = list(title = ""),
          yaxis = list(title = "Total Portfolio Expected Loss ($)", tickformat = ",.0f"),
          legend = list(orientation = "h", x = 0, y = 1.12)
        )
    })

    output$portfolio_summary_table <- DT::renderDataTable({
      data <- portfolio_summary_data()
      if (is.null(data) || nrow(data) == 0) {
        return(
          DT::datatable(
            tibble::tibble(Message = "No results available - run the TRISK-PRISK analysis first"),
            options = list(dom = "t"),
            rownames = FALSE
          )
        )
      }
      DT::datatable(
        data,
        options = list(dom = "t", pageLength = 4, scrollX = TRUE),
        rownames = FALSE
      )
    })

    output$company_select_ui <- shiny::renderUI({
      if (!has_results()) {
        return(shiny::p("Run the analysis to select a company.", class = "text-muted"))
      }
      companies <- sort(results_reactive()$companies$company)
      shiny::selectizeInput(
        ns("selected_company"),
        label = "Company",
        choices = companies,
        selected = companies[[1]],
        multiple = FALSE
      )
    })

    output$expected_loss_grouped_plot <- plotly::renderPlotly({
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

      row <- results_reactive()$companies |>
        dplyr::filter(.data$company == input$selected_company)
      if (nrow(row) == 0) return(plotly::plot_ly())

      plot_data <- tibble::tibble(
        pathway = c("Baseline", "Shock", "Baseline", "Shock"),
        group = c("TRISK-only", "TRISK-only", "TRISK+PRISK", "TRISK+PRISK"),
        expected_loss = c(
          row$expected_loss_trisk_baseline, row$expected_loss_trisk_shock,
          row$expected_loss_trisk_prisk_baseline, row$expected_loss_trisk_prisk_shock
        )
      )

      plotly::plot_ly(
        data = plot_data, x = ~pathway, y = ~expected_loss, color = ~group,
        colors = c("TRISK-only" = "#002776", "TRISK+PRISK" = "#FF6B35"),
        type = "bar"
      ) |>
        plotly::layout(
          barmode = "group",
          xaxis = list(title = ""),
          yaxis = list(title = "Expected Loss ($)", tickformat = ",.0f"),
          legend = list(orientation = "h", x = 0, y = 1.12)
        )
    })

    results_table_raw <- shiny::reactive({
      if (!has_results()) return(NULL)
      df <- results_reactive()$companies

      fmt_num <- function(x) round(x, 0)
      fmt_pct <- function(x) round(x * 100, 3)

      df |>
        dplyr::transmute(
          "Company" = .data$company,
          "NPV (TRISK baseline)" = fmt_num(.data$npv_trisk_baseline),
          "NPV (TRISK shock)" = fmt_num(.data$npv_trisk_shock),
          "NPV (TRISK+PRISK baseline)" = fmt_num(.data$npv_trisk_prisk_baseline),
          "NPV (TRISK+PRISK shock)" = fmt_num(.data$npv_trisk_prisk_shock),
          "PD % (TRISK baseline)" = fmt_pct(.data$pd_trisk_baseline),
          "PD % (TRISK shock)" = fmt_pct(.data$pd_trisk_shock),
          "PD % (TRISK+PRISK baseline)" = fmt_pct(.data$pd_trisk_prisk_baseline),
          "PD % (TRISK+PRISK shock)" = fmt_pct(.data$pd_trisk_prisk_shock),
          "Expected Loss (TRISK baseline)" = fmt_num(.data$expected_loss_trisk_baseline),
          "Expected Loss (TRISK shock)" = fmt_num(.data$expected_loss_trisk_shock),
          "Expected Loss (TRISK+PRISK baseline)" = fmt_num(.data$expected_loss_trisk_prisk_baseline),
          "Expected Loss (TRISK+PRISK shock)" = fmt_num(.data$expected_loss_trisk_prisk_shock)
        )
    })

    results_table_data <- shiny::reactive({
      data <- results_table_raw()
      if (is.null(data)) return(NULL)

      fmt_curr <- function(x) paste0("$", formatC(round(x), format = "f", digits = 0, big.mark = ","))

      data |>
        dplyr::transmute(
          "Company" = .data$Company,
          "NPV (TRISK baseline)" = fmt_curr(.data$`NPV (TRISK baseline)`),
          "NPV (TRISK shock)" = fmt_curr(.data$`NPV (TRISK shock)`),
          "NPV (TRISK+PRISK baseline)" = fmt_curr(.data$`NPV (TRISK+PRISK baseline)`),
          "NPV (TRISK+PRISK shock)" = fmt_curr(.data$`NPV (TRISK+PRISK shock)`),
          "PD % (TRISK baseline)" = .data$`PD % (TRISK baseline)`,
          "PD % (TRISK shock)" = .data$`PD % (TRISK shock)`,
          "PD % (TRISK+PRISK baseline)" = .data$`PD % (TRISK+PRISK baseline)`,
          "PD % (TRISK+PRISK shock)" = .data$`PD % (TRISK+PRISK shock)`,
          "Expected Loss (TRISK baseline)" = fmt_curr(.data$`Expected Loss (TRISK baseline)`),
          "Expected Loss (TRISK shock)" = fmt_curr(.data$`Expected Loss (TRISK shock)`),
          "Expected Loss (TRISK+PRISK baseline)" = fmt_curr(.data$`Expected Loss (TRISK+PRISK baseline)`),
          "Expected Loss (TRISK+PRISK shock)" = fmt_curr(.data$`Expected Loss (TRISK+PRISK shock)`)
        )
    })

    output$results_table <- DT::renderDataTable({
      data <- results_table_data()
      if (is.null(data) || nrow(data) == 0) {
        return(
          DT::datatable(
            tibble::tibble(Message = "No results available - run the TRISK-PRISK analysis first"),
            options = list(dom = "t"),
            rownames = FALSE
          )
        )
      }
      DT::datatable(
        data,
        options = list(pageLength = 25, scrollX = TRUE),
        rownames = FALSE
      )
    })

    output$download_results_csv <- shiny::downloadHandler(
      filename = function() paste0("trisk_prisk_results_", Sys.Date(), ".csv"),
      content = function(file) {
        data <- results_table_raw()
        if (is.null(data) || nrow(data) == 0) {
          utils::write.csv(data.frame(message = "No results available"), file, row.names = FALSE)
        } else {
          utils::write.csv(as.data.frame(data), file, row.names = FALSE)
        }
      }
    )
  })
}
