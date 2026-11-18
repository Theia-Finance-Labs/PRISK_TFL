# Tests for read_trisk_output, join_trisk_to_prisk_assets

write_trisk_fixture_csv <- function(path) {
  lines <- c(
    "run_id;asset_id;asset_name;company_id;company_name;country_iso2;sector;technology;year;net_profits_baseline_scenario;net_profits_shock_scenario;;;",
    "r1;A1;Asset One;C1;Company One;BR;Power;GasCap;2025; 500,000.00 ; 500,000.00 ;;;",
    "r1;A1;Asset One;C1;Company One;BR;Power;GasCap;2026; 700,000.00 ; 690,000.00 ;;;",
    "r1;A1;Asset One;C1;Company One;BR;Power;GasCap;2027; 714,000.00   ; 700,000.00 ;;;",
    "r1;A1;Asset One;C1;Company One;BR;Power;GasCap;2051;;;;;",
    "r1;A2;Asset Two;C2;Company Two;NL;Retail;;2026; 1,234,567.89 ; 1,200,000.00 ;;;",
    "r1;A3;Asset Three;C3;Company Three;BR;Retail;;2026; 100,000.00 ;                 -     ;;;"
  )
  writeLines(lines, path)
}

testthat::test_that("read_trisk_output parses semicolon-delimited, comma-thousands numbers correctly", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  write_trisk_fixture_csv(tmp)

  result <- read_trisk_output(tmp)

  testthat::expect_s3_class(result, "data.frame")
  # 2025 and 2051 rows are outside 2026-2050 and must be dropped
  testthat::expect_equal(sort(unique(result$year)), c(2026L, 2027L))
  testthat::expect_equal(nrow(result), 4)

  # Trailing unnamed columns from the header's trailing ";;;" must be dropped
  testthat::expect_false(any(grepl("^(\\.\\.\\.)?\\d+$", names(result))))

  # All original metadata columns are retained
  expected_cols <- c(
    "run_id", "asset_id", "asset_name", "company_id", "company_name",
    "country_iso2", "sector", "technology", "year",
    "net_profits_baseline_scenario", "net_profits_shock_scenario"
  )
  testthat::expect_true(all(expected_cols %in% names(result)))

  # " 700,000.00 " -> 700000 (comma thousands separator stripped, not comma-decimal)
  row_2026 <- result[result$asset_id == "A1" & result$year == 2026, ]
  testthat::expect_equal(row_2026$net_profits_baseline_scenario, 700000)
  testthat::expect_equal(row_2026$net_profits_shock_scenario, 690000)

  # " 1,234,567.89 " -> 1234567.89 (multiple thousands separators)
  row_a2 <- result[result$asset_id == "A2", ]
  testthat::expect_equal(row_a2$net_profits_baseline_scenario, 1234567.89)

  # "-" is accounting notation for zero (seen in the real TRISK export) and must
  # become 0, not NA -- a genuine zero-profit result must not be silently dropped.
  row_a3 <- result[result$asset_id == "A3", ]
  testthat::expect_equal(row_a3$net_profits_shock_scenario, 0)
  testthat::expect_false(is.na(row_a3$net_profits_shock_scenario))
})

testthat::test_that("read_trisk_output errors on missing required columns", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c("asset_id;year", "A1;2026"), tmp)

  testthat::expect_error(read_trisk_output(tmp), "missing required column")
})

testthat::test_that("read_trisk_output errors on unsupported file type", {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp), add = TRUE)
  writeLines("not a real trisk file", tmp)

  testthat::expect_error(read_trisk_output(tmp), "Unsupported")
})

testthat::test_that("read_trisk_output errors on missing file", {
  testthat::expect_error(
    read_trisk_output("/no/such/file/company_trajectories.csv"),
    "not found"
  )
})

testthat::test_that("join_trisk_to_prisk_assets joins on asset_id+company_id and adds asset/company aliases", {
  trisk_df <- tibble::tibble(
    asset_id = c("A1", "A1"),
    company_id = c("C1", "C1"),
    year = c(2026, 2027),
    net_profits_baseline_scenario = c(100, 110),
    net_profits_shock_scenario = c(90, 95)
  )
  prisk_assets <- tibble::tibble(
    asset = "A1",
    company = "C1",
    asset_category = "industrial building",
    share_of_economic_activity = 1
  )

  result <- join_trisk_to_prisk_assets(trisk_df, prisk_assets)

  testthat::expect_equal(nrow(result), 2)
  testthat::expect_true(all(c("asset", "company", "asset_category") %in% names(result)))
  testthat::expect_equal(unique(result$asset), "A1")
  testthat::expect_equal(unique(result$company), "C1")
  testthat::expect_equal(unique(result$asset_category), "industrial building")
})

testthat::test_that("join_trisk_to_prisk_assets warns and excludes unmatched asset_id/company_id combinations", {
  trisk_df <- tibble::tibble(
    asset_id = c("A1", "A_UNKNOWN"),
    company_id = c("C1", "C_UNKNOWN"),
    year = c(2026, 2026),
    net_profits_baseline_scenario = c(100, 200),
    net_profits_shock_scenario = c(90, 190)
  )
  prisk_assets <- tibble::tibble(
    asset = "A1",
    company = "C1",
    asset_category = "industrial building"
  )

  testthat::expect_warning(
    result <- join_trisk_to_prisk_assets(trisk_df, prisk_assets),
    "no matching PRISK asset"
  )
  testthat::expect_equal(nrow(result), 1)
  testthat::expect_equal(result$asset_id, "A1")
})

testthat::test_that("join_trisk_to_prisk_assets errors when nothing matches at all", {
  trisk_df <- tibble::tibble(
    asset_id = "A_UNKNOWN",
    company_id = "C_UNKNOWN",
    year = 2026,
    net_profits_baseline_scenario = 100,
    net_profits_shock_scenario = 90
  )
  prisk_assets <- tibble::tibble(asset = "A1", company = "C1", asset_category = "industrial building")

  testthat::expect_warning(
    testthat::expect_error(
      join_trisk_to_prisk_assets(trisk_df, prisk_assets),
      "no TRISK rows matched"
    ),
    "no matching PRISK asset"
  )
})
