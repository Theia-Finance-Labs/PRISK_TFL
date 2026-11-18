# Tests for mod_trisk_prisk_outputs: $-formatted currency display and the
# portfolio-level Expected Loss (Baseline vs. Shock, TRISK-only vs.
# TRISK+PRISK) bar chart data.

testthat::test_that("portfolio_summary_data $-prefixes totals and portfolio_summary_raw sums EL across companies", {
  testthat::skip_if_not_installed("shiny")

  companies <- tibble::tibble(
    company = c("C1", "C2"),
    npv_trisk_baseline = c(1000, 2000),
    npv_trisk_shock = c(800, 1600),
    npv_trisk_prisk_baseline = c(900, 1800),
    npv_trisk_prisk_shock = c(600, 1200),
    pd_trisk_baseline = c(0.01, 0.02),
    pd_trisk_shock = c(0.02, 0.03),
    pd_trisk_prisk_baseline = c(0.015, 0.025),
    pd_trisk_prisk_shock = c(0.03, 0.04),
    expected_loss_trisk_baseline = c(100, 200),
    expected_loss_trisk_shock = c(150, 250),
    expected_loss_trisk_prisk_baseline = c(120, 220),
    expected_loss_trisk_prisk_shock = c(240, 330)
  )
  results <- list(companies = companies)

  shiny::testServer(mod_trisk_prisk_outputs_server, args = list(
    id = "test",
    results_reactive = shiny::reactive(results)
  ), {
    data <- portfolio_summary_data()

    testthat::expect_true(all(grepl("^\\$", data$`Total NPV`)))
    testthat::expect_true(all(grepl("^\\$", data$`Total Expected Loss`)))
    testthat::expect_equal(data$`Total NPV`[data$Scenario == "TRISK-only Baseline"], "$3,000")
    testthat::expect_equal(data$`Total Expected Loss`[data$Scenario == "TRISK-only Baseline"], "$300")
    testthat::expect_false("EL % Change (Baseline to Shock)" %in% names(data))

    # portfolio_summary_raw feeds the new portfolio-level EL bar chart --
    # totals summed across every company, one row per (group, pathway).
    raw <- portfolio_summary_raw()
    testthat::expect_setequal(raw$group, c("TRISK-only", "TRISK+PRISK"))
    testthat::expect_setequal(raw$pathway, c("Baseline", "Shock"))
    testthat::expect_equal(
      raw$total_el[raw$group == "TRISK-only" & raw$pathway == "Baseline"], 300
    )
    testthat::expect_equal(
      raw$total_el[raw$group == "TRISK-only" & raw$pathway == "Shock"], 400
    )
    testthat::expect_equal(
      raw$total_el[raw$group == "TRISK+PRISK" & raw$pathway == "Baseline"], 340
    )
    testthat::expect_equal(
      raw$total_el[raw$group == "TRISK+PRISK" & raw$pathway == "Shock"], 570
    )
  })
})

testthat::test_that("results_table_data $-prefixes NPV/EL and no longer includes EL % change columns", {
  testthat::skip_if_not_installed("shiny")

  companies <- tibble::tibble(
    company = "C1",
    npv_trisk_baseline = 1000,
    npv_trisk_shock = 800,
    npv_trisk_prisk_baseline = 900,
    npv_trisk_prisk_shock = 600,
    pd_trisk_baseline = 0.01,
    pd_trisk_shock = 0.02,
    pd_trisk_prisk_baseline = 0.015,
    pd_trisk_prisk_shock = 0.03,
    expected_loss_trisk_baseline = 100,
    expected_loss_trisk_shock = 150,
    expected_loss_trisk_prisk_baseline = 120,
    expected_loss_trisk_prisk_shock = 240
  )
  results <- list(companies = companies)

  shiny::testServer(mod_trisk_prisk_outputs_server, args = list(
    id = "test",
    results_reactive = shiny::reactive(results)
  ), {
    data <- results_table_data()

    testthat::expect_equal(data$`NPV (TRISK baseline)`, "$1,000")
    testthat::expect_equal(data$`Expected Loss (TRISK baseline)`, "$100")
    testthat::expect_false("EL % Change (TRISK-only)" %in% names(data))
    testthat::expect_false("EL % Change (TRISK+PRISK)" %in% names(data))

    # Raw (unformatted) reactive used for CSV export stays numeric
    raw <- results_table_raw()
    testthat::expect_equal(raw$`NPV (TRISK baseline)`, 1000)
    testthat::expect_false("EL % Change (TRISK-only)" %in% names(raw))
  })
})
