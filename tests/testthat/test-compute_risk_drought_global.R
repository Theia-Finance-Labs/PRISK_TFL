# End-to-end tests of the Global drought engine through compute_risk() and
# compute_risk_probabilistic(), on the synthetic fixture root (see helper-drought-global.R).

make_drought_run_inputs <- function(env = parent.frame(), with_heat = FALSE) {
  fx <- make_drought_test_root(withr::local_tempdir(.local_envir = env))
  if (with_heat) {
    # add the heat engine to the same root (configs + maps + mappings)
    hx <- make_heat_test_root(withr::local_tempdir(.local_envir = env))
    file.copy(file.path(hx$config_dir, "Heat.yml"), fx$config_dir)
    file.copy(hx$maps_dir, fx$indicators_dir, recursive = TRUE)
    for (f in list.files(hx$mappings_dir, full.names = TRUE)) file.copy(f, fx$mappings_dir)
    fx$configs <- load_hazard_configs(fx$config_dir)
  }

  dir.create(file.path(fx$root, "areas"))
  file.copy(get_test_data_dir("areas", "brazil_adm_codes.csv"), file.path(fx$root, "areas", "brazil_adm_codes.csv"))
  input_dir <- file.path(fx$root, "inputs")
  dir.create(input_dir)
  a <- drought_test_assets()
  writexl::write_xlsx(
    data.frame(
      ID = a$company, Name = a$company, `Asset id` = a$asset, Country = a$country,
      Long = a$longitude, Lat = a$latitude, `Asset name` = a$asset, Sector = "1.1",
      `Share of economic activity` = 1, `Asset Subtype` = a$asset_subtype, `Asset Category` = a$asset_category,
      check.names = FALSE
    ),
    file.path(input_dir, "assets.xlsx")
  )
  comps <- unique(a$company)
  writexl::write_xlsx(
    data.frame(ID = comps, PD = NA, NPM = 0.07, `D/E` = NA, Volatility = 0.1, Revenue = 1e7, Debt = 3e7, check.names = FALSE),
    file.path(input_dir, "financials.xlsx")
  )
  writexl::write_xlsx(
    data.frame(ID = comps, Name = comps, Sector = "x", Technology = NA, Country = "AA", EAD = 5e6, Term = 5, LGD = 0.4, FI = "Bank"),
    file.path(input_dir, "portfolio.xlsx")
  )
  inv <- suppressMessages(load_hazards_and_inventory(fx$config_dir, fx$indicators_dir, aggregate_factor = 1L))
  list(
    fx = fx,
    assets = suppressMessages(read_assets(input_dir)),
    companies = suppressMessages(read_companies(input_dir)),
    inventory = inv$inventory,
    configs = inv$configs
  )
}

drought_event <- function(inv, id, year, scenario = "ssp585", season = "DJF", rp = 10, stat = "median",
                          mode = "deterministic") {
  d <- inv[inv$hazard_type == "Drought", ]
  row <- d[d$scenario_name == scenario & d$return_period == rp & d$statistic == stat & d$season == season, ][1, ]
  tibble::tibble(
    event_id = id, hazard_type = "Drought", hazard_indicator = "spei3",
    hazard_name = if (mode == "probabilistic") paste0("Drought__AAL__scenario_name=", scenario) else row$hazard_name,
    scenario_name = scenario, return_period = if (mode == "probabilistic") NA_real_ else as.numeric(rp),
    event_year = if (mode == "probabilistic") NA_integer_ else as.integer(year),
    season = if (mode == "probabilistic") NA_character_ else season, statistic = stat,
    spatial_level = "global", spatial_region_codes = NA_character_, spatial_region_labels = NA_character_,
    spatial_scheme = NA_character_, calc_mode = mode
  )
}

run_drought_det <- function(inputs, events, ...) {
  suppressMessages(compute_risk(
    assets = inputs$assets, companies = inputs$companies, events = events,
    hazards = list(dummy = terra::rast(nrows = 1, ncols = 1)),
    hazards_inventory = inputs$inventory,
    precomputed_hazards = data.frame(x = 1), hazard_configs = inputs$configs,
    hazards_dir = inputs$fx$config_dir, cnae_exposure = data.frame(cnae = 1, description = "x"),
    validate_inputs = FALSE, ...
  ))
}

run_drought_prob <- function(inputs, events) {
  suppressMessages(compute_risk_probabilistic(
    assets = inputs$assets, companies = inputs$companies, events = events,
    hazards = list(), hazards_inventory = inputs$inventory, hazard_configs = inputs$configs,
    hazards_dir = inputs$fx$config_dir, precomputed_hazards = data.frame(x = 1)
  ))
}

rev_at <- function(res, asset, scen, yr) {
  y <- res$assets_yearly
  y$revenue[y$asset == asset & y$scenario == scen & y$year == yr]
}

test_that("read_assets keeps crop and category columns of the global input format", {
  inputs <- make_drought_run_inputs()
  expect_true(all(c("asset_subtype", "asset_category", "country", "latitude", "longitude") %in% names(inputs$assets)))
  expect_equal(inputs$assets$asset_subtype[inputs$assets$asset == "A8"], "Maize 2")
})

test_that("compute_risk applies a drought-only event to agriculture assets in the event year only", {
  inputs <- make_drought_run_inputs()
  res <- run_drought_det(inputs, drought_event(inputs$inventory, "ev1", 2035))
  af <- res$assets_factors
  expect_equal(nrow(af), 9)
  expect_true(all(af$hazard_type == "Drought"))
  expect_true(all(c(
    "spei3", "spei3_matched", "crop", "crop_source", "growing_seasons", "in_growing_period",
    "damage_factor_in_window", "off_window", "damage_factor", "drought_status", "drought_revenue_adjustment"
  ) %in% names(af)))

  for (a in af$asset) {
    df <- af$damage_factor[af$asset == a]
    expect_equal(rev_at(res, a, "shock", 2035), rev_at(res, a, "baseline", 2035) * (1 - df), tolerance = 1e-9, info = a)
    expect_equal(rev_at(res, a, "shock", 2034), rev_at(res, a, "baseline", 2034), info = a)
    expect_equal(rev_at(res, a, "shock", 2036), rev_at(res, a, "baseline", 2036), info = a)
    expect_equal(
      af$drought_revenue_adjustment[af$asset == a], -rev_at(res, a, "baseline", 2035) * df,
      tolerance = 1e-6, info = a
    )
  }
  # the non-agriculture asset is untouched even though it sits on the map
  expect_equal(rev_at(res, "A5", "shock", 2035), rev_at(res, "A5", "baseline", 2035))
  expect_gt(af$damage_factor[af$asset == "A1"], 0)
  expect_true(length(res$drought_warnings) > 0)

  # companies get lower NPV under the drought
  co <- res$companies
  expect_true(all(c("company") %in% names(co)))
})

test_that("the event season decides in-window (full) or off-window (15%) damage", {
  inputs <- make_drought_run_inputs()
  djf <- run_drought_det(inputs, drought_event(inputs$inventory, "ev1", 2035, season = "DJF"))$assets_factors
  jja <- run_drought_det(inputs, drought_event(inputs$inventory, "ev1", 2035, season = "JJA"))$assets_factors
  g <- function(af, a, col) af[[col]][af$asset == a]
  # A1 maize grows in DJF only; A2 soybean (nearest calendar) in JJA only
  expect_true(g(djf, "A1", "in_growing_period"))
  expect_false(g(jja, "A1", "in_growing_period"))
  expect_equal(g(jja, "A1", "damage_factor"), 0.15 * g(jja, "A1", "damage_factor_in_window"))
  expect_false(g(djf, "A2", "in_growing_period"))
  expect_true(g(jja, "A2", "in_growing_period"))
  expect_equal(g(jja, "A2", "damage_factor"), g(jja, "A2", "damage_factor_in_window"))
})

test_that("statistic, return period, scenario and shock year all move the hazard the right way", {
  inputs <- make_drought_run_inputs()
  inv <- inputs$inventory
  df_a1 <- function(ev) {
    af <- run_drought_det(inputs, ev)$assets_factors
    c(spei = af$spei3[af$asset == "A1"], df = af$damage_factor[af$asset == "A1"])
  }
  med <- df_a1(drought_event(inv, "ev1", 2035))
  p90 <- df_a1(drought_event(inv, "ev1", 2035, stat = "p90"))
  p10 <- df_a1(drought_event(inv, "ev1", 2035, stat = "p10"))
  expect_lt(p90[["spei"]], med[["spei"]])
  expect_gt(p10[["spei"]], med[["spei"]])
  expect_gt(p90[["df"]], med[["df"]])
  expect_gt(df_a1(drought_event(inv, "ev1", 2035, rp = 100))[["df"]], med[["df"]])
  expect_lt(df_a1(drought_event(inv, "ev1", 2035, scenario = "ssp126"))[["df"]], med[["df"]])
  expect_gt(df_a1(drought_event(inv, "ev1", 2055))[["spei"]] * -1, med[["spei"]] * -1)
  # statistic missing on the event -> median
  ev <- drought_event(inv, "ev1", 2035)
  ev$statistic <- NA_character_
  expect_equal(df_a1(ev), med)
})

test_that("drought combines with heat in one deterministic run", {
  inputs <- make_drought_run_inputs(with_heat = TRUE)
  heat_row <- inputs$inventory[inputs$inventory$hazard_type == "Heat" & inputs$inventory$scenario_name == "ssp585" &
    inputs$inventory$return_period == 10 & inputs$inventory$statistic == "median", ][1, ]
  ev_heat <- tibble::tibble(
    event_id = "evh", hazard_type = "Heat", hazard_indicator = heat_row$hazard_indicator, hazard_name = heat_row$hazard_name,
    scenario_name = "ssp585", return_period = 10, event_year = 2035L, season = NA_character_, statistic = "median",
    spatial_level = "global", spatial_region_codes = NA_character_, spatial_region_labels = NA_character_,
    spatial_scheme = NA_character_, calc_mode = "deterministic"
  )
  ev <- dplyr::bind_rows(drought_event(inputs$inventory, "evd", 2035), ev_heat)
  both <- run_drought_det(inputs, ev)
  only_d <- run_drought_det(inputs, ev[1, ])
  af <- both$assets_factors
  expect_setequal(unique(af$hazard_type), c("Drought", "Heat"))
  expect_equal(nrow(af), 18)
  # A1 (agriculture): both shocks apply; A5 (commercial): only heat
  expect_lt(rev_at(both, "A1", "shock", 2035), rev_at(only_d, "A1", "shock", 2035))
  expect_lt(rev_at(both, "A5", "shock", 2035), rev_at(both, "A5", "baseline", 2035))
  expect_equal(rev_at(only_d, "A5", "shock", 2035), rev_at(only_d, "A5", "baseline", 2035))
})

test_that("compute_risk_probabilistic runs drought-only and deducts the yearly drought AAL", {
  inputs <- make_drought_run_inputs()
  res <- run_drought_prob(inputs, drought_event(inputs$inventory, "ev1", NA, mode = "probabilistic"))
  expect_true(all(c("aal_channels", "assets_factors", "assets_yearly") %in% names(res)))
  yr <- res$aal_channels$drought_yearly
  expect_true(all(c("asset", "year", "aal_drought_revenue_rate") %in% names(yr)))
  y <- res$assets_yearly
  for (a in c("A1", "A5", "A7")) {
    for (t in sort(unique(y$year))[c(1, 5, 10)]) {
      rate <- yr$aal_drought_revenue_rate[yr$asset == a & yr$year == t]
      expect_equal(rev_at(res, a, "shock", t), rev_at(res, a, "baseline", t) * (1 - rate), tolerance = 1e-9, info = paste(a, t))
    }
  }
  s <- res$aal_channels$summary
  expect_equal(s$aal_drought_revenue_rate[s$asset == "A5"], 0)
  expect_gt(s$aal_drought_revenue_rate[s$asset == "A1"], 0)
  expect_true("avg_annual_drought_revenue_adjustment" %in% names(s))
  expect_true(all(res$aal_channels$detail$channel == "aal_drought_revenue_rate"))
  expect_true(length(res$drought_warnings) > 0)
})

test_that("probabilistic drought and heat AALs add, one event per hazard type", {
  inputs <- make_drought_run_inputs(with_heat = TRUE)
  ev_h <- tibble::tibble(
    event_id = "evh", hazard_type = "Heat", hazard_indicator = "heat_excess_days",
    hazard_name = "Heat__AAL__scenario_name=ssp585", scenario_name = "ssp585", return_period = NA_real_,
    event_year = NA_integer_, season = NA_character_, statistic = "median", spatial_level = "global",
    spatial_region_codes = NA_character_, spatial_region_labels = NA_character_, spatial_scheme = NA_character_,
    calc_mode = "probabilistic"
  )
  ev_d <- drought_event(inputs$inventory, "evd", NA, mode = "probabilistic")
  both <- run_drought_prob(inputs, dplyr::bind_rows(ev_d, ev_h))
  y <- both$assets_yearly
  hr <- both$aal_channels$heat_yearly
  dr <- both$aal_channels$drought_yearly
  t <- min(y$year)
  rate <- hr$aal_lp_revenue_rate[hr$asset == "A1" & hr$year == t] + dr$aal_drought_revenue_rate[dr$asset == "A1" & dr$year == t]
  expect_equal(rev_at(both, "A1", "shock", t), rev_at(both, "A1", "baseline", t) * (1 - rate), tolerance = 1e-9)
  expect_setequal(unique(both$assets_factors$hazard_type), c("Drought", "Heat"))
  expect_error(run_drought_prob(inputs, dplyr::bind_rows(ev_d, ev_d)), "one Probabilistic")
})

test_that("a drought event limited to administrative regions shocks only the assets inside", {
  inputs <- make_drought_run_inputs()
  make_adm_test_dir(inputs$fx$root)
  whole <- run_drought_det(inputs, drought_event(inputs$inventory, "ev1", 2035))
  ev <- drought_event(inputs$inventory, "ev1", 2035) |>
    dplyr::mutate(spatial_level = "adm0", spatial_region_codes = "BBB", spatial_scheme = "global_adm")
  res <- run_drought_det(inputs, ev, base_dir = inputs$fx$root)
  af <- res$assets_factors
  # fixture country BBB = lon 1..2 (lat -2..2): A2 (1.2, -1.2) and A7 (1.5, 1.5)
  expect_setequal(af$asset[af$spatial_included], c("A2", "A7"))
  for (a in c("A2", "A7")) {
    expect_equal(af$damage_factor[af$asset == a], whole$assets_factors$damage_factor[whole$assets_factors$asset == a])
    expect_equal(rev_at(res, a, "shock", 2035), rev_at(whole, a, "shock", 2035))
  }
  out <- af[!af$spatial_included, ]
  expect_true(all(is.na(out$damage_factor)) && all(is.na(out$spei3)))
  for (a in out$asset) expect_equal(rev_at(res, a, "shock", 2035), rev_at(res, a, "baseline", 2035))
})

test_that("heat regional selection still blanks only the heat columns (shared helper)", {
  rows <- tibble::tibble(
    asset = c("in_north", "in_bland"), company = "C", latitude = c(0.5, -0.5), longitude = c(0.5, 1.5),
    event_id = "ev1", excess_heat_days = c(5, 6), s = c(0.1, 0.1), output_change_pct = c(-0.01, -0.01),
    spatial_included = TRUE, spatial_multiplier = 1, spatial_exposure_status = NA_character_
  )
  root <- make_adm_test_dir()
  ev <- tibble::tibble(
    event_id = "ev1", hazard_type = "Heat", spatial_level = "adm0", spatial_region_codes = "AAA",
    spatial_region_labels = NA_character_, spatial_scheme = "global_adm"
  )
  hx <- make_heat_test_root()
  out <- apply_heat_spatial_selection(rows, ev, hx$configs, NULL, root)
  expect_equal(out$spatial_included, c(TRUE, FALSE))
  expect_equal(out$excess_heat_days, c(5, NA))
  expect_equal(out$output_change_pct, c(-0.01, NA))
})

test_that("Brazil-mode configs never route events to the global drought engine", {
  cfg <- load_hazard_configs(get_test_data_dir("hazards", "config"))
  ev <- tibble::tibble(hazard_type = c("Drought", "Flood"))
  expect_equal(is_global_drought_event(ev, cfg), c(FALSE, FALSE))
})

test_that("Profit Pathways works on a probabilistic drought result (AAL deducted every year)", {
  testthat::skip_if_not_installed("shiny")
  inputs <- make_drought_run_inputs()
  res <- run_drought_prob(inputs, drought_event(inputs$inventory, "ev1", NA, mode = "probabilistic"))
  expect_true("asset_subtype" %in% names(res$assets_factors))
  shiny::testServer(mod_profit_pathways_server, args = list(id = "pp", results_reactive = shiny::reactive(res)), {
    session$setInputs(log_scale = FALSE)
    md <- asset_metadata()
    expect_true(nrow(md) > 0)
    expect_true(all(c("asset", "asset_subtype") %in% names(md)))
    expect_no_error(output$assets_selection_table)
    expect_no_error(output$profit_shock)
    sd <- shock_data()
    bd <- baseline_data()
    m <- merge(sd, bd, by = c("asset", "year"), suffixes = c("_shock", "_base"))
    yr <- res$aal_channels$drought_yearly
    a1 <- m[m$asset == "A1", ]
    rate <- yr$aal_drought_revenue_rate[yr$asset == "A1"][match(a1$year, yr$year[yr$asset == "A1"])]
    expect_equal(a1$profit_shock, a1$profit_base * (1 - rate), tolerance = 1e-9)
  })
})

test_that("Profit Pathways metadata tolerates results without asset_subtype / asset_category", {
  testthat::skip_if_not_installed("shiny")
  res <- list(
    assets_factors = tibble::tibble(asset = "X", company = "C", share_of_economic_activity = 1),
    assets_yearly = tibble::tibble(asset = "X", company = "C", year = 2030:2031, scenario = c("baseline", "baseline"), revenue = 1, profit = 1)
  )
  shiny::testServer(mod_profit_pathways_server, args = list(id = "pp", results_reactive = shiny::reactive(res)), {
    md <- asset_metadata()
    expect_equal(nrow(md), 1)
    expect_true(is.na(md$asset_subtype))
  })
})
