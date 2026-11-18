#' trisk_prisk_asset_analysis UI Function
#'
#' @description Module to display the Average Annual Loss (AAL) computed for
#'   each analyzed asset -- the same per-asset, per-scenario AAL figures that
#'   feed the physical-risk adjustment and, ultimately, the Outputs tab.
#' @param id Internal parameter for shiny
#' @export
mod_trisk_prisk_asset_analysis_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "results-container",
      shiny::h3("PRISK Analysis: Average Annual Loss by Asset", class = "results-title"),
      shiny::p(
        "The Average Annual Loss (AAL) computed for each asset under each flood scenario, integrated across every available return period. These are the figures deducted from the TRISK profit pathways in the TRISK+PRISK scenarios.",
        class = "text-muted", style = "margin-bottom: 1rem;"
      ),
      shiny::div(
        class = "results-downloads",
        shiny::downloadButton(ns("download_aal_csv"), "Download AAL by Asset (CSV)", class = "btn btn-info")
      ),
      shiny::div(
        class = "events-table-container",
        style = "margin-top: 0.5rem;",
        DT::dataTableOutput(ns("aal_table"))
      )
    )
  )
}

#' trisk_prisk_asset_analysis Server Functions
#'
#' @param id Internal parameter for shiny
#' @param results_reactive reactive returning the compute_risk_trisk_prisk() result list (or NULL)
#' @param trisk_joined_reactive reactive returning the joined TRISK-PRISK asset table (for company/asset_category display)
#' @export
mod_trisk_prisk_asset_analysis_server <- function(id, results_reactive, trisk_joined_reactive = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    aal_table_raw <- shiny::reactive({
      results <- results_reactive()
      if (is.null(results) || is.null(results$aal) || is.null(results$aal$endpoints)) {
        return(NULL)
      }
      endpoints <- results$aal$endpoints

      asset_meta <- NULL
      if (!is.null(trisk_joined_reactive)) {
        trisk_df <- try(trisk_joined_reactive(), silent = TRUE)
        if (!inherits(trisk_df, "try-error") && !is.null(trisk_df) && nrow(trisk_df) > 0) {
          asset_meta <- trisk_df |>
            dplyr::select(dplyr::any_of(c("asset", "company", "asset_category"))) |>
            dplyr::distinct(.data$asset, .keep_all = TRUE)
        }
      }

      long <- dplyr::bind_rows(
        dplyr::mutate(endpoints$present, scenario = "Present"),
        dplyr::mutate(endpoints$rcp26, scenario = "RCP2.6"),
        dplyr::mutate(endpoints$rcp85, scenario = "RCP8.5")
      )

      if (!is.null(asset_meta)) {
        long <- long |> dplyr::left_join(asset_meta, by = "asset")
      }

      long |>
        dplyr::transmute(
          "Asset" = .data$asset,
          "Company" = if ("company" %in% names(long)) .data$company else NA_character_,
          "Asset Category" = if ("asset_category" %in% names(long)) .data$asset_category else NA_character_,
          "Scenario" = .data$scenario,
          "AAL Physical Damage - Profit ($)" = round(.data$aal_pd_profit, 0),
          "AAL Physical Damage - Revenue Rate (%)" = round(.data$aal_pd_revenue_rate * 100, 3),
          "AAL Business Disruption - Revenue Rate (%)" = round(.data$aal_bd_revenue_rate * 100, 3)
        ) |>
        dplyr::arrange(.data$Asset, factor(.data$Scenario, levels = c("Present", "RCP2.6", "RCP8.5")))
    })

    aal_table_data <- shiny::reactive({
      data <- aal_table_raw()
      if (is.null(data)) return(NULL)

      fmt_curr <- function(x) paste0("$", formatC(round(x), format = "f", digits = 0, big.mark = ","))

      data |>
        dplyr::mutate("AAL Physical Damage - Profit ($)" = fmt_curr(.data$`AAL Physical Damage - Profit ($)`))
    })

    output$aal_table <- DT::renderDataTable({
      data <- aal_table_data()
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

    output$download_aal_csv <- shiny::downloadHandler(
      filename = function() paste0("trisk_prisk_aal_by_asset_", Sys.Date(), ".csv"),
      content = function(file) {
        data <- aal_table_raw()
        if (is.null(data) || nrow(data) == 0) {
          utils::write.csv(data.frame(message = "No results available"), file, row.names = FALSE)
        } else {
          utils::write.csv(as.data.frame(data), file, row.names = FALSE)
        }
      }
    )
  })
}
