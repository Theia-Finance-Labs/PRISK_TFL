testthat::test_that("app_ui exposes upload, run, download controls", {
  ui <- app_ui(request = NULL)
  html <- htmltools::renderTags(ui)$html

  # Namespaced module IDs
  testthat::expect_true(grepl("control-select_folder", html))
  testthat::expect_true(grepl("control-run_analysis", html))
  testthat::expect_true(grepl("profit_pathways-download_profit_pathways_csv", html))
})

testthat::test_that("app_ui exposes a Region toggle and has no footer logo", {
  ui <- app_ui(request = NULL)
  html <- htmltools::renderTags(ui)$html

  testthat::expect_true(grepl("id=\"region\"", html))
  testthat::expect_true(grepl("Global", html))
  testthat::expect_false(grepl("partner-logo", html))
  testthat::expect_false(grepl("app-footer", html))
  testthat::expect_false(grepl("FiBras_II_Coop_BaCen_Curupira_Theia", html))
})
