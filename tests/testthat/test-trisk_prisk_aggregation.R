# Phase 4: aggregation to company level for the TRISK-PRISK integration mode.
# No new aggregation code exists for this -- apply_trisk_prisk_adjustment()'s
# output is fed directly through the existing discount_yearly_profits() and
# aggregate_assets_to_company(), unmodified. This test exercises that full glue
# with a synthetic *multi-asset* company, since the real TRISK/PRISK test data
# only has one asset per company and wouldn't exercise the summing behavior.

testthat::test_that("TRISK-PRISK pathway aggregates correctly across multiple assets of one company", {
  # Company X1 has two assets: a building (industrial) and an agricultural asset.
  trisk_joined <- tibble::tibble(
    asset = c("X1_building", "X1_farm"),
    company = c("X1", "X1"),
    asset_category = c("industrial building", "agriculture"),
    year = c(2026, 2026),
    net_profits_baseline_scenario = c(1000, 500),
    net_profits_shock_scenario = c(1000, 500)
  )
  aal_by_year <- tibble::tibble(
    asset = rep(c("X1_building", "X1_farm"), each = 2),
    year = 2026,
    pathway = rep(c("baseline", "shock"), 2),
    aal_pd_profit = c(20, 20, 0, 0),
    aal_pd_revenue_rate = c(0, 0, 0.04, 0.04),
    aal_bd_revenue_rate = c(0.05, 0.05, 0.03, 0.03)
  )
  companies <- tibble::tibble(company = "X1", net_profit_margin = 0.1)

  adjusted <- apply_trisk_prisk_adjustment(trisk_joined, aal_by_year, companies)
  discounted <- discount_yearly_profits(adjusted, discount_rate = 0.05, base_year = 2026)
  company_yearly <- aggregate_assets_to_company(discounted)

  # Hand-computed trisk_prisk_baseline profit per asset:
  # Building: revenue=1000/0.1=10000; revenue_adj=10000*(1-0.05)=9500;
  #   profit_adj=9500*0.1=950; profit_final=950-20=930
  # Farm: revenue=500/0.1=5000; revenue_adj=5000*(1-0.03-0.04)=4650;
  #   profit_final=4650*0.1=465 (no separate pd_profit deduction)
  expected_building <- 930
  expected_farm <- 465
  expected_company_total <- expected_building + expected_farm

  company_prisk_baseline <- company_yearly[
    company_yearly$company == "X1" & company_yearly$scenario == "trisk_prisk_baseline" & company_yearly$year == 2026,
  ]
  testthat::expect_equal(company_prisk_baseline$total_profit, expected_company_total)

  # trisk_baseline (unadjusted) should just be the sum of the two raw TRISK profits
  company_trisk_baseline <- company_yearly[
    company_yearly$company == "X1" & company_yearly$scenario == "trisk_baseline" & company_yearly$year == 2026,
  ]
  testthat::expect_equal(company_trisk_baseline$total_profit, 1000 + 500)

  # All four scenarios must be present at company level, not collapsed/conflated
  testthat::expect_setequal(
    unique(company_yearly$scenario[company_yearly$company == "X1"]),
    c("trisk_baseline", "trisk_shock", "trisk_prisk_baseline", "trisk_prisk_shock")
  )

  # discounted_net_profit at the base year (2026) equals undiscounted profit
  testthat::expect_equal(
    company_prisk_baseline$total_discounted_net_profit,
    company_prisk_baseline$total_profit
  )
})

testthat::test_that("TRISK-PRISK aggregation keeps multiple companies' pathways independent", {
  trisk_joined <- tibble::tibble(
    asset = c("Y1_asset", "Z1_asset"),
    company = c("Y1", "Z1"),
    asset_category = c("industrial building", "commercial building"),
    year = c(2026, 2026),
    net_profits_baseline_scenario = c(1000, 2000),
    net_profits_shock_scenario = c(1000, 2000)
  )
  aal_by_year <- tibble::tibble(
    asset = rep(c("Y1_asset", "Z1_asset"), each = 2),
    year = 2026,
    pathway = rep(c("baseline", "shock"), 2),
    aal_pd_profit = 0, aal_pd_revenue_rate = 0, aal_bd_revenue_rate = 0
  )
  companies <- tibble::tibble(company = c("Y1", "Z1"), net_profit_margin = c(0.1, 0.1))

  adjusted <- apply_trisk_prisk_adjustment(trisk_joined, aal_by_year, companies)
  discounted <- discount_yearly_profits(adjusted, discount_rate = 0.05, base_year = 2026)
  company_yearly <- aggregate_assets_to_company(discounted)

  y1 <- company_yearly[company_yearly$company == "Y1" & company_yearly$scenario == "trisk_prisk_baseline", ]
  z1 <- company_yearly[company_yearly$company == "Z1" & company_yearly$scenario == "trisk_prisk_baseline", ]

  # Zero AAL everywhere -> trisk_prisk should equal the raw TRISK profit, and
  # Y1/Z1 must not bleed into each other's totals.
  testthat::expect_equal(y1$total_profit, 1000)
  testthat::expect_equal(z1$total_profit, 2000)
})
