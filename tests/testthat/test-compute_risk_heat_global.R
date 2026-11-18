# End-to-end tests of the Global heat engine through compute_risk() and
# compute_risk_probabilistic(), on the synthetic fixture root (see helper-heat-global.R).

make_heat_run_inputs <- function(env = parent.frame()) {
  fx <- make_heat_test_root(withr::local_tempdir(.local_envir = env))

  # read_assets() looks for ../areas/brazil_adm_codes.csv next to the input folder
  dir.create(file.path(fx$root, "areas"))
  file.copy(get_test_data_dir("areas", "brazil_adm_codes.csv"), file.path(fx$root, "areas", "brazil_adm_codes.csv"))
  input_dir <- file.path(fx$root, "inputs")
  dir.create(input_dir)

  writexl::write_xlsx(
    data.frame(
      ID = c("C1", "C1", "C2"),
      Name = c("C1", "C1", "C2"),
      `Asset id` = c("A1", "A2", "A3"),
      Country = c("AA", "BBB", "AA"),
      Long = c(0.5, 1.5, -0.5),
      Lat = c(0.5, -0.5, 1.5),
      `Asset name` = c("A1", "A2", "A3"),
      Sector = c("1.1", "10.1.1", "64"),
      `Share of economic activity` = 1,
      `Asset Category` = "commercial building",
      check.names = FALSE
    ),
    file.path(input_dir, "assets.xlsx")
  )
  writexl::write_xlsx(
    data.frame(ID = c("C1", "C2"), PD = NA, NPM = 0.07, `D/E` = NA, Volatility = 0.1, Revenue = 1e7, Debt = 3e7, check.names = FALSE),
    file.path(input_dir, "financials.xlsx")
  )
  writexl::write_xlsx(
    data.frame(
      ID = c("C1", "C2"), Name = c("C1", "C2"), Sector = "x", Technology = NA, Country = "AA",
      EAD = 5e6, Term = 5, LGD = 0.4, FI = "Bank"
    ),
    file.path(input_dir, "portfolio.xlsx")
  )

  inv <- load_hazards_and_inventory(fx$config_dir, fx$indicators_dir, aggregate_factor = 1L)
  list(
    fx = fx,
    assets = suppressMessages(read_assets(input_dir)),
    companies = suppressMessages(read_companies(input_dir)),
    inventory = inv$inventory,
    configs = inv$configs
  )
}

heat_event <- function(inv, id, year, scenario = "ssp585", rp = 10, stat = "median",
                       mode = "deterministic") {
  row <- inv[inv$scenario_name == scenario & inv$return_period == rp & inv$statistic == stat, ][1, ]
  ev <- tibble::tibble(
    event_id = id, hazard_type = "Heat", hazard_indicator = row$hazard_indicator,
    hazard_name = if (mode == "probabilistic") paste0("Heat__AAL__scenario_name=", scenario) else row$hazard_name,
    scenario_name = scenario, return_period = if (mode == "probabilistic") NA_real_ else as.numeric(rp),
    event_year = if (mode == "probabilistic") NA_integer_ else as.integer(year),
    season = NA_character_, statistic = stat,
    spatial_level = "global", spatial_region_codes = NA_character_, spatial_region_labels = NA_character_,
    spatial_scheme = NA_character_, calc_mode = mode
  )
  ev
}

run_heat_det <- function(inputs, events, ...) {
  suppressMessages(compute_risk(
    assets = inputs$assets, companies = inputs$companies, events = events,
    hazards = list(dummy = terra::rast(nrows = 1, ncols = 1)), # structurally required, never queried for heat
    hazards_inventory = inputs$inventory,
    precomputed_hazards = data.frame(x = 1), hazard_configs = inputs$configs,
    hazards_dir = inputs$fx$config_dir, cnae_exposure = data.frame(cnae = 1, description = "x"),
    validate_inputs = FALSE, ...
  ))
}

test_that("compute_risk applies a heat-only event and matches the closed-form revenue change", {
  inputs <- make_heat_run_inputs()
  ev <- heat_event(inputs$inventory, "ev1", 2035, rp = 10)
  res <- run_heat_det(inputs, ev)

  expect_true(all(c("assets_factors", "companies", "assets_yearly", "companies_yearly") %in% names(res)))
  af <- res$assets_factors
  expect_equal(nrow(af), 3)
  expect_true(all(af$hazard_type == "Heat"))
  expect_true(all(c(
    "excess_heat_days", "lp_exposure", "lp_loss", "nace_code", "statistic", "hazard_return_period", "country",
    "imf_group", "beta_L", "beta_L_source", "s", "output_change_pct", "baseline_revenue", "revenue_adjustment"
  ) %in% names(af)))

  excess <- 0.55 * heat_test_value("median", 1, 10) + 0.45 * heat_test_value("median", 2, 10)
  expect_equal(af$excess_heat_days[af$asset == "A1"], excess)
  expect_equal(af$lp_exposure[af$asset == "A1"], "High")
  expect_equal(af$lp_exposure[af$asset == "A2"], "Low")

  y <- res$assets_yearly
  rev_of <- function(asset, scen, yr) y$revenue[y$asset == asset & y$scenario == scen & y$year == yr]
  for (a in c("A1", "A2", "A3")) {
    lp <- af$lp_loss[af$asset == a]
    ex <- af$excess_heat_days[af$asset == a]
    beta <- af$beta_L[af$asset == a]
    expect_equal(
      rev_of(a, "shock", 2035) / rev_of(a, "baseline", 2035) - 1,
      (1 - ex / 365 * lp)^beta - 1,
      tolerance = 1e-12
    )
    # the reported adjustment is exactly what was applied: shock = baseline + revenue_adjustment
    expect_equal(af$baseline_revenue[af$asset == a], rev_of(a, "baseline", 2035))
    expect_equal(
      rev_of(a, "shock", 2035), rev_of(a, "baseline", 2035) + af$revenue_adjustment[af$asset == a], tolerance = 1e-9
    )
    # the shock is a single event-year shock, like every deterministic hazard
    expect_equal(rev_of(a, "shock", 2034), rev_of(a, "baseline", 2034))
    expect_equal(rev_of(a, "shock", 2036), rev_of(a, "baseline", 2036))
  }
})

test_that("beta_L follows the IMF group: Advanced 0.535, EMDE 0.399, unknown country EMDE default", {
  inputs <- make_heat_run_inputs()
  inputs$assets$country[[3]] <- "ZZ" # not in the IMF classification
  res <- run_heat_det(inputs, heat_event(inputs$inventory, "ev1", 2030, rp = 2))
  af <- res$assets_factors
  by <- function(a, col) af[[col]][af$asset == a]

  expect_equal(by("A1", "imf_group"), "Advanced Economies") # AA
  expect_equal(by("A1", "beta_L"), 0.535)
  expect_equal(by("A1", "beta_L_source"), "IMF_classification")
  expect_equal(by("A2", "imf_group"), "Emerging Market and Developing Economies") # BBB
  expect_equal(by("A2", "beta_L"), 0.399)
  expect_equal(by("A3", "beta_L"), 0.399) # default
  expect_equal(by("A3", "beta_L_source"), "default_EMDE")
  expect_equal(sum(af$beta_L_source == "default_EMDE"), 1)
  expect_true(any(grepl("default_EMDE", res$heat_warnings)))

  # country-level economic inputs play no role any more: the change is s and beta_L only
  y <- res$assets_yearly
  chg <- function(a) {
    y$revenue[y$asset == a & y$scenario == "shock" & y$year == 2030] /
      y$revenue[y$asset == a & y$scenario == "baseline" & y$year == 2030] - 1
  }
  for (a in af$asset) {
    expect_equal(chg(a), (1 - by(a, "excess_heat_days") / 365 * by(a, "lp_loss"))^by(a, "beta_L") - 1, tolerance = 1e-12)
    expect_equal(chg(a), by(a, "output_change_pct"), tolerance = 1e-12)
  }
})

test_that("Advanced economies lose more output than EMDE for the same labour shock", {
  inputs <- make_heat_run_inputs()
  inputs$assets$country <- c("AA", "BBB", "AA") # A1 / A3 Advanced, A2 EMDE
  inputs$assets$sector <- rep("1.1", 3) # same exposure class everywhere
  inputs$assets$latitude <- rep(0.5, 3)
  inputs$assets$longitude <- rep(0.5, 3) # same location -> same excess days
  res <- run_heat_det(inputs, heat_event(inputs$inventory, "ev1", 2040, rp = 10))
  af <- res$assets_factors
  expect_equal(length(unique(af$s)), 1)
  adv <- af$output_change_pct[af$asset == "A1"]
  emde <- af$output_change_pct[af$asset == "A2"]
  expect_lt(adv, emde)
  expect_equal(adv, (1 - af$s[[1]])^0.535 - 1, tolerance = 1e-12)
  expect_equal(emde, (1 - af$s[[1]])^0.399 - 1, tolerance = 1e-12)
})

test_that("the heat statistic changes the result and defaults to median", {
  inputs <- make_heat_run_inputs()
  med <- run_heat_det(inputs, heat_event(inputs$inventory, "ev1", 2030, rp = 5, stat = "median"))
  mean_ <- run_heat_det(inputs, heat_event(inputs$inventory, "ev1", 2030, rp = 5, stat = "mean"))
  expect_equal(unique(med$assets_factors$statistic), "median")
  expect_gt(
    mean_$assets_factors$excess_heat_days[mean_$assets_factors$asset == "A1"],
    med$assets_factors$excess_heat_days[med$assets_factors$asset == "A1"]
  )

  no_stat <- heat_event(inputs$inventory, "ev1", 2030, rp = 5)
  no_stat$statistic <- NULL
  def <- run_heat_det(inputs, no_stat)
  expect_equal(def$assets_factors$excess_heat_days, med$assets_factors$excess_heat_days)
})

test_that("a later shock year uses hotter maps and a bigger revenue hit", {
  inputs <- make_heat_run_inputs()
  hit <- function(year) {
    res <- run_heat_det(inputs, heat_event(inputs$inventory, "ev1", year, rp = 10))
    y <- res$assets_yearly
    y$revenue[y$asset == "A1" & y$scenario == "shock" & y$year == year] /
      y$revenue[y$asset == "A1" & y$scenario == "baseline" & y$year == year] - 1
  }
  expect_gt(hit(2030), hit(2040)) # less negative earlier
  expect_gt(hit(2040), hit(2049))
  # years up to 2030 all use the first map unchanged -> identical % impact
  expect_equal(hit(2027), hit(2030))
})

test_that("heat warnings are returned when sectors or countries cannot be matched", {
  inputs <- make_heat_run_inputs()
  inputs$assets$sector[[1]] <- "completely unknown"
  inputs$assets$country[[2]] <- "ZZ"
  res <- run_heat_det(inputs, heat_event(inputs$inventory, "ev1", 2030))
  expect_true(any(grepl("No NACE match", res$heat_warnings)))
  expect_true(any(grepl("default_EMDE", res$heat_warnings)))
  expect_equal(res$assets_factors$lp_exposure[res$assets_factors$asset == "A1"], HEAT_DEFAULT_EXPOSURE)

  clean <- make_heat_run_inputs()
  res2 <- run_heat_det(clean, heat_event(clean$inventory, "ev1", 2030))
  expect_null(res2$heat_warnings) # no extra output component when nothing needed a fallback
})

test_that("compute_risk_probabilistic runs heat-only and applies the yearly heat AAL", {
  inputs <- make_heat_run_inputs()
  ev <- heat_event(inputs$inventory, "ev1", NA, mode = "probabilistic")
  res <- suppressMessages(compute_risk_probabilistic(
    assets = inputs$assets, companies = inputs$companies, events = ev,
    hazards = list(), hazards_inventory = inputs$inventory, hazard_configs = inputs$configs,
    hazards_dir = inputs$fx$config_dir, precomputed_hazards = data.frame(x = 1)
  ))

  expect_true(all(c("assets_factors", "companies", "assets_yearly", "companies_yearly", "aal_channels") %in% names(res)))
  expect_true(all(c("summary", "detail", "heat_yearly") %in% names(res$aal_channels)))
  s <- res$aal_channels$summary
  expect_true(all(c("aal_pd_profit", "aal_pd_revenue_rate", "aal_bd_revenue_rate", "aal_lp_revenue_rate") %in% names(s)))
  expect_true(all(s$aal_pd_profit == 0 & s$aal_pd_revenue_rate == 0 & s$aal_bd_revenue_rate == 0))
  expect_true(all(s$aal_lp_revenue_rate > 0))

  # the shock revenue equals baseline * (1 - that year's heat AAL)
  y <- res$assets_yearly
  hy <- res$aal_channels$heat_yearly
  for (yr in c(2026, 2035, 2050)) {
    b <- y$revenue[y$asset == "A1" & y$scenario == "baseline" & y$year == yr]
    sh <- y$revenue[y$asset == "A1" & y$scenario == "shock" & y$year == yr]
    rate <- hy$aal_lp_revenue_rate[hy$asset == "A1" & hy$year == yr]
    expect_equal(sh, b * (1 - rate), tolerance = 1e-12)
  }
  # average annual revenue adjustment: mean over years of baseline revenue x (-AAL rate), negative = loss
  expect_true("avg_annual_revenue_adjustment" %in% names(s))
  base <- y[y$scenario == "baseline", c("asset", "year", "revenue")]
  for (a in c("A1", "A2", "A3")) {
    ya <- hy[hy$asset == a, ]
    b <- base$revenue[match(paste(a, ya$year), paste(base$asset, base$year))]
    expect_equal(ya$revenue_adjustment, -b * ya$aal_lp_revenue_rate, tolerance = 1e-9)
    expect_equal(s$avg_annual_revenue_adjustment[s$asset == a], mean(-b * ya$aal_lp_revenue_rate), tolerance = 1e-9)
    expect_lte(s$avg_annual_revenue_adjustment[s$asset == a], 0)
  }
  # the shock revenue is the baseline plus that year's adjustment
  expect_equal(
    y$revenue[y$asset == "A1" & y$scenario == "shock" & y$year == 2035],
    base$revenue[base$asset == "A1" & base$year == 2035] + hy$revenue_adjustment[hy$asset == "A1" & hy$year == 2035],
    tolerance = 1e-9
  )

  # heat risk grows over the horizon
  r <- hy[hy$asset == "A1", ]
  expect_gt(r$aal_lp_revenue_rate[r$year == 2050], r$aal_lp_revenue_rate[r$year == 2030])

  af <- res$assets_factors
  expect_true(all(af$hazard_type == "Heat"))
  expect_equal(nrow(af), 3 * 6) # assets x return periods
  expect_true(all(c("excess_heat_days", "s", "output_change_pct", "imf_group", "beta_L", "beta_L_source") %in% names(af)))
  expect_true(all(af$output_change_pct <= 0))
})

test_that("compute_risk_probabilistic allows one event per hazard type only", {
  inputs <- make_heat_run_inputs()
  two <- dplyr::bind_rows(
    heat_event(inputs$inventory, "ev1", NA, mode = "probabilistic"),
    heat_event(inputs$inventory, "ev2", NA, scenario = "ssp126", mode = "probabilistic")
  )
  expect_error(
    suppressMessages(compute_risk_probabilistic(
      assets = inputs$assets, companies = inputs$companies, events = two, hazards = list(),
      hazards_inventory = inputs$inventory, hazard_configs = inputs$configs,
      hazards_dir = inputs$fx$config_dir, precomputed_hazards = data.frame(x = 1)
    )),
    "one Probabilistic \\(AAL\\) event is supported per hazard type"
  )

  bogus <- dplyr::mutate(heat_event(inputs$inventory, "ev1", NA, mode = "probabilistic"), hazard_type = "Fire")
  expect_error(
    suppressMessages(compute_risk_probabilistic(
      assets = inputs$assets, companies = inputs$companies, events = bogus, hazards = list(),
      hazards_inventory = inputs$inventory, hazard_configs = inputs$configs,
      hazards_dir = inputs$fx$config_dir, precomputed_hazards = data.frame(x = 1)
    )),
    "only hazard types"
  )
})

test_that("heat events are excluded from raster extraction and spatial separation", {
  inputs <- make_heat_run_inputs()
  # a basin level on a heat event is ignored (basin separation is Flood-only): all assets are still shocked
  ev <- heat_event(inputs$inventory, "ev1", 2040, rp = 10)
  ev$spatial_level <- "level4"
  ev$spatial_scheme <- "global_basins"
  ev$spatial_region_codes <- "123"
  res <- run_heat_det(inputs, ev)
  expect_equal(sum(res$assets_factors$excess_heat_days > 0), 3)
  expect_true(all(res$assets_factors$spatial_included))
})

# ---------------------------------------------------------------------------
# Regional (administrative) selection of a heat event
# ---------------------------------------------------------------------------

heat_adm_event <- function(inputs, level, codes, labels = NA_character_, year = 2035) {
  heat_event(inputs$inventory, "ev1", year, rp = 10) |>
    dplyr::mutate(
      spatial_level = level, spatial_region_codes = codes, spatial_region_labels = labels,
      spatial_scheme = "global_adm"
    )
}

test_that("a heat event limited to regions shocks only the assets inside them", {
  inputs <- make_heat_run_inputs()
  make_adm_test_dir(inputs$fx$root)
  whole <- run_heat_det(inputs, heat_event(inputs$inventory, "ev1", 2035, rp = 10))
  # A1 and A3 are in country AAA / ADM1 North, A2 is in BBB (see helper-global-adm.R)
  expect_true(all(whole$assets_factors$spatial_included))
  expect_true(all(whole$assets_factors$excess_heat_days[whole$assets_factors$asset %in% c("A1", "A2")] > 0))

  scenarios <- list(
    list(level = "adm0", codes = "AAA", inside = c("A1", "A3")),
    list(level = "adm0", codes = "BBB", inside = "A2"),
    list(level = "adm0", codes = "AAA|BBB", inside = c("A1", "A2", "A3")),
    list(level = "adm1", codes = "ID1N", inside = c("A1", "A3")),
    list(level = "adm1", codes = "ID1O", inside = "A2"),
    list(level = "adm2", codes = "ID2T2", inside = c("A1", "A3")),
    list(level = "adm2", codes = "ID2T1", inside = character(0)),
    list(level = "adm2", codes = "ID2B1", inside = "A2")
  )
  for (sc in scenarios) {
    res <- run_heat_det(inputs, heat_adm_event(inputs, sc$level, sc$codes), base_dir = inputs$fx$root)
    af <- res$assets_factors
    info <- paste(sc$level, sc$codes)
    expect_setequal(af$asset[af$spatial_included], sc$inside)
    expect_equal(nrow(af), 3, info = info)

    # assets inside: exactly the whole-area values and revenue path
    for (a in sc$inside) {
      expect_equal(af$excess_heat_days[af$asset == a], whole$assets_factors$excess_heat_days[whole$assets_factors$asset == a], info = info)
      expect_equal(af$output_change_pct[af$asset == a], whole$assets_factors$output_change_pct[whole$assets_factors$asset == a], info = info)
      expect_equal(af$revenue_adjustment[af$asset == a], whole$assets_factors$revenue_adjustment[whole$assets_factors$asset == a], info = info)
    }
    rev <- function(r, a, scen) r$assets_yearly$revenue[r$assets_yearly$asset == a & r$assets_yearly$scenario == scen & r$assets_yearly$year == 2035]
    for (a in sc$inside) expect_equal(rev(res, a, "shock"), rev(whole, a, "shock"), info = info)

    # assets outside: reported as not exposed, no heat values, and no shock at all
    for (a in setdiff(c("A1", "A2", "A3"), sc$inside)) {
      row <- af[af$asset == a, ]
      expect_false(row$spatial_included, info = info)
      expect_equal(row$spatial_multiplier, 0)
      expect_equal(row$spatial_exposure_status, spatial_status_not_exposed())
      expect_true(is.na(row$excess_heat_days) && is.na(row$s) && is.na(row$output_change_pct) && is.na(row$revenue_adjustment))
      expect_equal(rev(res, a, "shock"), rev(res, a, "baseline"), info = info)
    }
  }
})

test_that("a regional heat event needs no extra setup: data are loaded from base_dir, or passed in", {
  inputs <- make_heat_run_inputs()
  make_adm_test_dir(inputs$fx$root)
  ev <- heat_adm_event(inputs, "adm0", "BBB")
  lazy <- run_heat_det(inputs, ev, base_dir = inputs$fx$root)
  preloaded <- run_heat_det(inputs, ev, spatial_separation_data = load_global_spatial_data(inputs$fx$root))
  expect_equal(lazy$assets_factors, preloaded$assets_factors)
  expect_equal(lazy$assets_yearly, preloaded$assets_yearly)
})

test_that("a regional selection that contains no asset leaves every revenue untouched", {
  inputs <- make_heat_run_inputs()
  make_adm_test_dir(inputs$fx$root)
  res <- run_heat_det(inputs, heat_adm_event(inputs, "adm2", "ID2B2"), base_dir = inputs$fx$root)
  expect_false(any(res$assets_factors$spatial_included))
  y <- res$assets_yearly
  wide <- tidyr::pivot_wider(y[, c("asset", "year", "scenario", "revenue")], names_from = "scenario", values_from = "revenue")
  expect_equal(wide$shock, wide$baseline)
})

test_that("a whole-area heat event is unchanged by the regional machinery", {
  inputs <- make_heat_run_inputs()
  ev <- heat_event(inputs$inventory, "ev1", 2035, rp = 10)
  plain <- run_heat_det(inputs, ev)
  make_adm_test_dir(inputs$fx$root)
  with_data <- run_heat_det(inputs, ev, base_dir = inputs$fx$root)
  expect_equal(plain$assets_yearly, with_data$assets_yearly)
  expect_equal(plain$assets_factors, with_data$assets_factors)
  expect_true(all(plain$assets_factors$spatial_included))
  expect_true(all(is.na(plain$assets_factors$spatial_exposure_status)))
})
