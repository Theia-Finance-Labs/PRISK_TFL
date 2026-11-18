test_that("heat_blend_weights reproduces the documented blending rules", {
  centres <- c(2030.5, 2040.5, 2050.5, 2060.5)
  w <- function(year) heat_blend_weights(year, centres)

  # years up to the first centre use the first map unchanged
  for (yr in c(2025, 2026, 2030)) {
    expect_equal(w(yr)$centre_year, 2030.5)
    expect_equal(w(yr)$weight, 1)
  }

  # documented examples: weight on the LATER map = (year - earlier centre) / 10
  expect_equal(w(2035)$weight, c(0.55, 0.45))
  expect_equal(w(2035)$centre_year, c(2030.5, 2040.5))
  expect_equal(w(2039)$weight, c(0.15, 0.85))
  expect_equal(w(2045)$weight, c(0.55, 0.45))
  expect_equal(w(2045)$centre_year, c(2040.5, 2050.5))
  expect_equal(w(2050)$weight, c(0.05, 0.95))
  expect_equal(w(2050)$centre_year, c(2040.5, 2050.5))
  expect_equal(w(2031)$weight, c(0.95, 0.05))

  # weights always sum to one and the last map is held beyond its centre
  for (yr in 2025:2070) expect_equal(sum(w(yr)$weight), 1)
  expect_equal(w(2075)$centre_year, 2060.5)
  expect_equal(w(2075)$weight, 1)
})

test_that("heat_blend_weights handles degenerate input", {
  expect_equal(nrow(heat_blend_weights(2035, numeric(0))), 0)
  expect_equal(nrow(heat_blend_weights(NA, c(2030.5, 2040.5))), 0)
  expect_equal(heat_blend_weights(2040.5, c(2030.5, 2040.5, 2050.5))$weight, 1)
})

test_that("normalize_nace_code maps all supported input shapes to the table's form", {
  expect_equal(normalize_nace_code("1.1000000000000001"), "1.1") # Excel float artefact
  expect_equal(normalize_nace_code(c("10", "18.1.4", "22.1.9", "23.6.4")), c("10", "18.1.4", "22.1.9", "23.6.4"))
  expect_equal(normalize_nace_code(c("01.11", "0111", "1011", "10.11")), c("1.1.1", "1.1.1", "10.1.1", "10.1.1"))
  expect_equal(normalize_nace_code(c("01", "1", "01.1", "10.1")), c("1", "1", "1.1", "10.1"))
  expect_equal(normalize_nace_code("1.30"), "1.3.0")
  expect_equal(normalize_nace_code(c("abc", "", NA, "N/A", "1.2.3.4")), rep(NA_character_, 5))
  expect_equal(normalize_nace_code(c(" 10 ", "10.1.1")), c("10", "10.1.1"))
})

test_that("load_nace_exposure_table reads and normalises the exposure table", {
  fx <- make_heat_test_root()
  tbl <- load_nace_exposure_table(fx$mappings_dir)
  expect_true(all(c("code", "description", "level", "exposure") %in% names(tbl)))
  expect_setequal(unique(tbl$exposure), c("High", "Medium", "Low"))
  expect_true("10.1.1" %in% tbl$code)
  expect_error(load_nace_exposure_table(fx$mappings_dir, "missing.xlsx"), "not found")
})

test_that("resolve_nace_exposure matches code, parent code, description, then defaults", {
  fx <- make_heat_test_root()
  tbl <- load_nace_exposure_table(fx$mappings_dir)
  res <- resolve_nace_exposure(
    c("1.1000000000000001", "10.1.1", "10.1.7", "1.1.9", "Manufacture of food products", "no such sector", NA, "64"),
    tbl
  )

  expect_equal(res$nace_match, c("code", "code", "parent_code", "parent_code", "description", "default", "default", "code"))
  # parents: 10.1.7 -> 10.1 (group); 1.1.9 is not a listed class -> 1.1 (group)
  expect_equal(res$nace_code, c("1.1", "10.1.1", "10.1", "1.1", "10", NA, NA, "64"))
  expect_equal(res$lp_exposure, c("High", "Low", "Low", "High", "Medium", HEAT_DEFAULT_EXPOSURE, HEAT_DEFAULT_EXPOSURE, "Low"))
  expect_equal(res$nace_level[1:2], c("Group", "Class"))
  expect_equal(res$nace_description[[2]], "Processing and preserving of meat")
})

test_that("resolve_nace_exposure prefers the broadest level for duplicated descriptions", {
  tbl <- tibble::tibble(
    code = c("5", "5.1.1"),
    description = c("Same words", "Same words"),
    level = c("Division", "Class"),
    exposure = c("High", "Low")
  )
  res <- resolve_nace_exposure("same words", tbl)
  expect_equal(res$nace_code, "5")
  expect_equal(res$lp_exposure, "High")
})

test_that("heat_lp_loss_for_exposure applies the hardcoded 20.8 / 11.8 / 4.2 percent", {
  expect_equal(heat_lp_loss_for_exposure(c("High", "medium", "LOW")), c(0.208, 0.118, 0.042))
  expect_true(is.na(heat_lp_loss_for_exposure("unknown")))
  expect_equal(HEAT_LP_LOSS_BY_EXPOSURE[["high"]], 0.208)
  expect_equal(HEAT_LP_LOSS_BY_EXPOSURE[["medium"]], 0.118)
  expect_equal(HEAT_LP_LOSS_BY_EXPOSURE[["low"]], 0.042)
})

test_that("labour shock and output change reproduce the specified worked example", {
  # high-exposure asset: 30 additional hot days, 21% daily productivity loss
  s <- heat_labour_shock(30, 0.21)
  expect_equal(s, 30 / 365 * 0.21)
  expect_equal(round(s, 6), 0.017260)

  emde <- heat_output_change_pct(s, 0.399)
  advanced <- heat_output_change_pct(s, 0.535)
  expect_equal(emde, (1 - s)^0.399 - 1)
  expect_equal(advanced, (1 - s)^0.535 - 1)
  expect_equal(round(100 * emde, 4), -0.6923) # EMDE, beta_L = 0.399
  expect_equal(round(100 * advanced, 4), -0.9272) # Advanced, beta_L = 0.535
  expect_lt(advanced, emde) # larger elasticity -> larger loss
})

test_that("with no labour shock the output change is exactly zero", {
  expect_identical(heat_output_change_pct(heat_labour_shock(0, 0.208), 0.399), 0)
  expect_identical(heat_output_change_pct(heat_labour_shock(0, 0.208), 0.535), 0)
  expect_identical(heat_output_change_pct(0, 0.399), 0)
  expect_identical(heat_output_change_pct(c(0, 0), c(0.399, 0.535)), c(0, 0))
})

test_that("heat_beta_l_parameters reads beta_L from the config and validates it", {
  fx <- make_heat_test_root()
  expect_equal(heat_beta_l_parameters(fx$configs$Heat), c(advanced = 0.535, emde = 0.399))

  # parameters are swappable in the config (sensitivity runs) without touching code
  root2 <- withr::local_tempdir()
  fx2 <- make_heat_test_root(root2)
  writeLines(heat_test_config_yaml(beta_advanced = 0.6, beta_emde = 0.3), file.path(fx2$config_dir, "Heat.yml"))
  expect_equal(heat_beta_l_parameters(load_hazard_configs(fx2$config_dir)$Heat), c(advanced = 0.6, emde = 0.3))

  cfg <- fx$configs$Heat
  cfg$shocks$revenue$equations[[1]]$constants$beta_L_emde <- NULL
  expect_error(heat_beta_l_parameters(cfg), "beta_L_emde")
  cfg2 <- fx$configs$Heat
  cfg2$shocks$revenue$equations[[1]]$constants$beta_L_advanced <- 1.5
  expect_error(heat_beta_l_parameters(cfg2), "in \\(0, 1\\]")
})

test_that("load_country_code_table keeps only identifiers and drops Global-AVG", {
  fx <- make_heat_test_root()
  tbl <- load_country_code_table(fx$mappings_dir)
  expect_equal(names(tbl), c("iso2", "iso3", "name"))
  expect_equal(nrow(tbl), 4)
  expect_false("Global-AVG" %in% tbl$name)
})

test_that("load_imf_classification attaches ISO codes by exact name, then the explicit alias file", {
  fx <- make_heat_test_root()
  imf <- load_imf_classification(fx$mappings_dir)
  expect_equal(nrow(imf), 5)
  expect_setequal(unique(imf$imf_group), c(IMF_GROUP_ADVANCED, IMF_GROUP_EMDE))

  # exact name match in the country code table
  alpha <- imf[imf$imf_name == "Alphaland", ]
  expect_equal(c(alpha$iso2, alpha$iso3, alpha$code_source), c("AA", "AAA", "country_code_table"))
  # spelling differs ("The Delta Republic" vs "Deltaland"): only the alias file connects them
  delta <- imf[imf$imf_name == "The Delta Republic", ]
  expect_equal(c(delta$iso2, delta$iso3, delta$code_source), c("DD", "DDD", "alias_file"))
  # absent from the code table but listed in the alias file
  expect_equal(imf$iso2[imf$imf_name == "Zetaland"], "ZE")

  # without the alias file nothing is guessed: the alias-only countries simply have no code
  file.remove(file.path(fx$mappings_dir, "imf_country_aliases.csv"))
  no_alias <- load_imf_classification(fx$mappings_dir)
  expect_true(is.na(no_alias$iso2[no_alias$imf_name == "The Delta Republic"]))
  expect_equal(no_alias$iso2[no_alias$imf_name == "Alphaland"], "AA")
})

test_that("load_imf_classification rejects unknown labels and duplicated countries", {
  fx <- make_heat_test_root()
  bad <- heat_test_imf_table()
  bad$Classification[[1]] <- "Frontier Economies"
  writexl::write_xlsx(bad, file.path(fx$mappings_dir, "IMF_classification.xlsx"))
  expect_error(load_imf_classification(fx$mappings_dir), "Unknown IMF classification label")

  dup <- rbind(heat_test_imf_table(), heat_test_imf_table()[1, ])
  writexl::write_xlsx(dup, file.path(fx$mappings_dir, "IMF_classification.xlsx"))
  expect_error(load_imf_classification(fx$mappings_dir), "more than once")
  expect_error(load_imf_classification(file.path(fx$mappings_dir, "nope")), "not found")
})

test_that("resolve_imf_beta_l assigns beta_L by IMF group and flags the EMDE default", {
  fx <- make_heat_test_root()
  imf <- load_imf_classification(fx$mappings_dir)
  codes <- load_country_code_table(fx$mappings_dir)
  beta <- c(advanced = 0.535, emde = 0.399)

  res <- resolve_imf_beta_l(
    c("AA", "bbb", "Alphaland", "The Delta Republic", "Deltaland", "DD", "Zetaland", "XX", NA, "", "uk"),
    imf, codes, beta
  )
  expect_equal(res$imf_match, c(
    "iso2", "iso3", "imf_name", "imf_name", "country_code_table_name", "iso2", "imf_name",
    "default_EMDE", "default_EMDE", "default_EMDE", "default_EMDE"
  ))
  expect_equal(res$imf_group[1:7], c(
    IMF_GROUP_ADVANCED, IMF_GROUP_EMDE, IMF_GROUP_ADVANCED, IMF_GROUP_EMDE, IMF_GROUP_EMDE, IMF_GROUP_EMDE, IMF_GROUP_ADVANCED
  ))
  expect_equal(res$beta_L[1:7], c(0.535, 0.399, 0.535, 0.399, 0.399, 0.399, 0.535))
  expect_equal(res$beta_L_source[1:7], rep(HEAT_BETA_SOURCE_IMF, 7))

  # not in the classification (or missing) -> EMDE default, visibly flagged
  fallback <- res[8:11, ]
  expect_equal(fallback$imf_group, rep(IMF_GROUP_EMDE, 4))
  expect_equal(fallback$beta_L, rep(0.399, 4))
  expect_equal(fallback$beta_L_source, rep(HEAT_BETA_SOURCE_DEFAULT, 4))
  expect_equal(HEAT_BETA_SOURCE_DEFAULT, "default_EMDE")
  expect_true(all(is.na(fallback$imf_country)))

  # beta_L values follow the parameter list, not constants in the code
  swapped <- resolve_imf_beta_l(c("AA", "BBB"), imf, codes, c(advanced = 0.6, emde = 0.3))
  expect_equal(swapped$beta_L, c(0.6, 0.3))
})

test_that("country matching is exact: no fuzzy or accent-folded matches", {
  fx <- make_heat_test_root()
  imf <- load_imf_classification(fx$mappings_dir)
  codes <- load_country_code_table(fx$mappings_dir)
  beta <- c(advanced = 0.535, emde = 0.399)
  res <- resolve_imf_beta_l(c("Alphalan", "Alpha land", "Alphaland ", "ALPHALAND"), imf, codes, beta)
  expect_equal(res$beta_L_source, c(HEAT_BETA_SOURCE_DEFAULT, HEAT_BETA_SOURCE_DEFAULT, HEAT_BETA_SOURCE_IMF, HEAT_BETA_SOURCE_IMF))

  # accents are matched exactly as written (UTF-8 safe), never folded
  accent <- tibble::tibble(
    imf_name = c("T\u00fcrkiye", "C\u00f4te d'Ivoire"), imf_group = IMF_GROUP_EMDE,
    iso2 = c("TR", "CI"), iso3 = c("TUR", "CIV"), code_source = "alias_file"
  )
  r2 <- resolve_imf_beta_l(c("T\u00fcrkiye", "Turkiye", "TR", "C\u00f4te d'Ivoire"), accent, codes, beta)
  expect_equal(r2$beta_L_source, c(HEAT_BETA_SOURCE_IMF, HEAT_BETA_SOURCE_DEFAULT, HEAT_BETA_SOURCE_IMF, HEAT_BETA_SOURCE_IMF))
})

test_that("scan_global_heat_maps finds windows, centres and bands", {
  fx <- make_heat_test_root()
  sc <- scan_global_heat_maps(fx$maps_dir)

  expect_equal(nrow(sc$files), 6) # 2 scenarios x 3 windows
  expect_setequal(unique(sc$files$ssp), c("ssp126", "ssp585"))
  ssp585 <- sc$files[sc$files$ssp == "ssp585", ]
  expect_equal(ssp585$centre_year, c(2030.5, 2040.5, 2050.5))
  expect_equal(nrow(sc$bands), 4 * 6)
  expect_setequal(unique(sc$bands$statistic), HEAT_STATISTICS)
  expect_setequal(unique(sc$bands$return_period), heat_test_rps())

  expect_equal(nrow(scan_global_heat_maps(file.path(fx$root, "nope"))$files), 0)
})

test_that("build_global_heat_inventory lists scenario x return period x statistic", {
  fx <- make_heat_test_root()
  ind <- fx$configs$Heat$indicators$heat_excess_days
  inv <- build_global_heat_inventory("Heat", "heat_excess_days", ind, fx$indicators_dir)

  expect_equal(nrow(inv), 2 * 6 * 4)
  expect_setequal(unique(inv$statistic), HEAT_STATISTICS)
  expect_true(is.integer(inv$return_period)) # matches the flood inventory type
  expect_equal(anyDuplicated(inv$indicator_key), 0)
  expect_equal(anyDuplicated(inv$hazard_name), 0)
  # the index suffix appears exactly once in the hazard name
  expect_equal(lengths(regmatches(inv$hazard_name, gregexpr("scenario_name=", inv$hazard_name))), rep(1L, nrow(inv)))
  expect_equal(
    inv$hazard_name[inv$scenario_name == "ssp585" & inv$return_period == 10L & inv$statistic == "median"],
    "Heat__heat_global__excess_days__scenario_name=ssp585__return_period=10__statistic=median"
  )
  expect_true(all(inv$hazard_type == "Heat"))
  expect_true(all(c("scenario_name", "return_period", "statistic", "hazard_name", "indicator_key") %in% names(inv)))
})

test_that("global heat config is recognised and Brazil-style configs are not", {
  fx <- make_heat_test_root()
  expect_true(is_global_heat_config(fx$configs$Heat))
  expect_equal(global_heat_hazard_types(fx$configs), "Heat")
  expect_false(is_global_heat_config(list(indicators = list(a = list(type = NA_character_)))))
  expect_false(is_global_heat_config(NULL))

  ev <- tibble::tibble(hazard_type = c("Heat", "Flood", "Heat"))
  expect_equal(is_global_heat_event(ev, fx$configs), c(TRUE, FALSE, TRUE))
  expect_length(is_global_heat_event(tibble::tibble(), fx$configs), 0)
})

test_that("load_hazards_and_inventory registers heat rows without rasters", {
  fx <- make_heat_test_root()
  hz <- load_hazards_and_inventory(fx$config_dir, fx$indicators_dir, aggregate_factor = 1L)

  expect_length(hz$hazards, 0) # heat maps are read lazily at compute time
  expect_equal(unique(hz$inventory$hazard_type), "Heat")
  expect_equal(nrow(hz$inventory), 48)
  ui <- filter_inventory_for_ui(hz$inventory, hz$configs)
  expect_equal(nrow(ui), 48)
  expect_equal(order_heat_statistics(c("p90", "mean", "p10", "median")), c("median", "mean", "p10", "p90"))
})

test_that("extract_global_heat_excess reads the right band per window and clamps", {
  fx <- make_heat_test_root()
  assets <- heat_test_assets()

  ex <- extract_global_heat_excess(assets, fx$maps_dir, "ssp585", statistic = "median", return_periods = c(2, 100))
  a1 <- ex[ex$asset == "A1", ]
  expect_equal(a1$excess_days[a1$centre_year == 2030.5 & a1$return_period == 2], heat_test_value("median", 1, 2))
  expect_equal(a1$excess_days[a1$centre_year == 2050.5 & a1$return_period == 100], heat_test_value("median", 3, 100))

  # missing coordinates and a point outside the raster -> NA
  expect_true(all(is.na(ex$excess_days[ex$asset %in% c("A3", "A4")])))

  # negative values (p10 base is -50) are really stored as negatives in the maps ...
  raw <- terra::rast(file.path(fx$maps_dir, "hi35_change_ssp585_2021-2040.tif"))[["excess_p10_RP2"]]
  expect_lt(terra::global(raw, "min")[[1]], 0)
  # ... and are clamped to 0 on extraction (excess days cannot be negative), never above 365
  neg <- extract_global_heat_excess(assets, fx$maps_dir, "ssp585", statistic = "p10", return_periods = 2)
  expect_true(all(neg$excess_days[!is.na(neg$excess_days)] == 0))
  big <- extract_global_heat_excess(assets, fx$maps_dir, "ssp585", statistic = "mean", return_periods = 2)
  expect_true(all(big$excess_days[!is.na(big$excess_days)] <= 365))

  expect_error(extract_global_heat_excess(assets, fx$maps_dir, "ssp999"), "no heat maps")
  expect_error(extract_global_heat_excess(assets, fx$maps_dir, "ssp585", statistic = "max"), "statistic")
})

test_that("blend_global_heat_excess blends the bracketing windows linearly", {
  fx <- make_heat_test_root()
  assets <- heat_test_assets()
  ex <- extract_global_heat_excess(assets, fx$maps_dir, "ssp585", "mean", return_periods = c(2, 10))

  # 2035: 0.55 * window1 + 0.45 * window2
  b <- blend_global_heat_excess(ex, 2035)
  a1_rp2 <- b$excess_days[b$asset == "A1" & b$return_period == 2]
  expect_equal(a1_rp2, 0.55 * heat_test_value("mean", 1, 2) + 0.45 * heat_test_value("mean", 2, 2))

  # 2026 -> first window unchanged; 2055 -> windows 3 (centre 2050.5) held constant
  b26 <- blend_global_heat_excess(ex, 2026)
  expect_equal(b26$excess_days[b26$asset == "A1" & b26$return_period == 10], heat_test_value("mean", 1, 10))
  b55 <- blend_global_heat_excess(ex, 2055)
  expect_equal(b55$excess_days[b55$asset == "A1" & b55$return_period == 10], heat_test_value("mean", 3, 10))

  # NA propagates (no coordinates) instead of becoming a silent 0
  expect_true(is.na(b$excess_days[b$asset == "A3"][[1]]))
})

test_that("the heat revenue equation applies the labour-only output change", {
  fx <- make_heat_test_root()
  cfg <- fx$configs$Heat

  s <- heat_labour_shock(c(30, 30, 0), c(0.21, 0.21, 0.21))
  d <- tibble::tibble(
    asset = c("X", "Y", "Z"), revenue = 1000, beta_L = c(0.399, 0.535, 0.399),
    output_change_pct = heat_output_change_pct(s, c(0.399, 0.535, 0.399))
  )
  out <- evaluate_hazard_shock(d, cfg, "revenue")
  expect_equal(out$shock_value, c(1000 * (1 + (1 - s[[1]])^0.399 - 1), 1000 * (1 + (1 - s[[2]])^0.535 - 1), 1000), tolerance = 1e-12)
  # baseline + revenue_adjustment, with revenue_adjustment = baseline x output_change_pct
  expect_equal(out$shock_value, 1000 + 1000 * d$output_change_pct, tolerance = 1e-12)
})

test_that("build_global_heat_event_factors builds one row per asset with the shock inputs", {
  fx <- make_heat_test_root()
  assets <- heat_test_assets()
  ev <- tibble::tibble(
    event_id = "ev1", hazard_type = "Heat", hazard_name = "Heat__test", scenario_name = "ssp585",
    return_period = 10, event_year = 2035L, statistic = "median"
  )
  res <- build_global_heat_event_factors(assets, ev, fx$configs, fx$config_dir)
  rows <- res$rows

  expect_equal(nrow(rows), 4)
  expect_setequal(rows$asset, assets$asset)
  expect_true(all(c(
    "excess_heat_days", "lp_exposure", "lp_loss", "nace_code", "nace_match", "country", "imf_group", "beta_L",
    "beta_L_source", "s", "output_change_pct", "event_id", "hazard_type", "hazard_return_period", "statistic",
    "heat_map_weights"
  ) %in% names(rows)))
  # the full Cobb-Douglas inputs are gone
  expect_false(any(c("L0", "K0", "E0", "cb_match", "cb_country") %in% names(rows)))

  r1 <- rows[rows$asset == "A1", ]
  expect_equal(r1$excess_heat_days, 0.55 * heat_test_value("median", 1, 10) + 0.45 * heat_test_value("median", 2, 10))
  expect_equal(r1$lp_exposure, "High") # NACE 1.1 (Excel float) -> Group -> High
  expect_equal(r1$lp_loss, 0.208)
  expect_equal(r1$imf_group, IMF_GROUP_ADVANCED) # country AA
  expect_equal(r1$beta_L, 0.535)
  expect_equal(r1$beta_L_source, HEAT_BETA_SOURCE_IMF)
  expect_equal(r1$s, r1$excess_heat_days / 365 * 0.208)
  expect_equal(r1$output_change_pct, (1 - r1$s)^0.535 - 1)

  r2 <- rows[rows$asset == "A2", ]
  expect_equal(r2$imf_group, IMF_GROUP_EMDE) # country BBB (ISO3)
  expect_equal(r2$beta_L, 0.399)
  expect_equal(r2$beta_L_source, HEAT_BETA_SOURCE_IMF)
  expect_equal(r2$lp_exposure, "Low") # NACE 10.1.1

  # unmatched country + sector fall back with warnings; missing heat data -> 0 days + warning
  r3 <- rows[rows$asset == "A3", ]
  expect_equal(r3$beta_L_source, HEAT_BETA_SOURCE_DEFAULT) # country ZZ is not in the classification
  expect_equal(r3$beta_L, 0.399)
  expect_equal(r3$nace_match, "default")
  expect_equal(r3$excess_heat_days, 0)
  expect_equal(r3$s, 0)
  expect_identical(r3$output_change_pct, 0)
  expect_true(any(grepl("NACE", res$warnings)))
  expect_true(any(grepl("default_EMDE", res$warnings)))
  expect_true(any(grepl("A3", res$warnings[grepl("heat-map value", res$warnings)])))
})

test_that("build_global_heat_event_factors defaults the statistic to median", {
  fx <- make_heat_test_root()
  ev <- tibble::tibble(
    event_id = "ev1", hazard_type = "Heat", hazard_name = "h", scenario_name = "ssp585",
    return_period = 2, event_year = 2030L
  )
  res <- build_global_heat_event_factors(heat_test_assets(), ev, fx$configs, fx$config_dir)
  expect_equal(unique(res$rows$statistic), "median")
  expect_equal(res$rows$excess_heat_days[res$rows$asset == "A1"], heat_test_value("median", 1, 2))
})

test_that("heat loss rates by return period equal the closed form", {
  fx <- make_heat_test_root()
  cfg <- fx$configs$Heat
  attrs <- tibble::tibble(asset = c("A", "B"), lp_loss = c(0.208, 0.042), beta_L = c(0.535, 0.399))
  excess <- tibble::tibble(
    asset = rep(c("A", "B"), each = 2), return_period = rep(c(2, 10), 2),
    excess_days = c(30, 90, 30, 90)
  )
  loss <- heat_loss_rates_by_rp(excess, attrs, cfg)
  expect_equal(nrow(loss), 4)
  expect_true(all(c("s", "output_change_pct", "loss") %in% names(loss)))
  a2 <- loss[loss$asset == "A" & loss$return_period == 2, ]
  expect_equal(a2$loss, 1 - (1 - 30 / 365 * 0.208)^0.535, tolerance = 1e-12)
  expect_equal(a2$s, 30 / 365 * 0.208)
  expect_equal(a2$output_change_pct, -a2$loss, tolerance = 1e-12)
  b10 <- loss$loss[loss$asset == "B" & loss$return_period == 10]
  expect_equal(b10, 1 - (1 - 90 / 365 * 0.042)^0.399, tolerance = 1e-12)
})

test_that("compute_global_heat_aal integrates per year with a flat RP2 -> RP1 extension", {
  fx <- make_heat_test_root()
  assets <- heat_test_assets()[1:2, ]
  ev <- tibble::tibble(
    event_id = "ev1", hazard_type = "Heat", hazard_name = "h", scenario_name = "ssp585",
    statistic = "median", calc_mode = "probabilistic"
  )
  years <- 2026:2030
  res <- compute_global_heat_aal(assets, ev, fx$configs, fx$config_dir, years)

  expect_setequal(names(res), c("summary", "yearly", "detail", "factors_by_rp", "warnings"))
  expect_equal(nrow(res$yearly), 2 * length(years))
  expect_equal(nrow(res$summary), 2)
  expect_equal(nrow(res$detail), 2 * 6)

  # independent re-computation for A1 in 2030 (first window only, weight 1)
  rps <- heat_test_rps()
  excess <- vapply(rps, function(rp) heat_test_value("median", 1, rp), numeric(1))
  loss <- 1 - (1 - excess / 365 * 0.208)^0.535 # A1: NACE 1.1 -> High, country AA -> Advanced (beta_L 0.535)
  expected <- calculate_aal(c(1, rps), c(loss[[1]], loss)) # flat from RP2 down to RP1
  got <- res$yearly$aal_lp_revenue_rate[res$yearly$asset == "A1" & res$yearly$year == 2030]
  expect_equal(got, expected, tolerance = 1e-12)

  # years at/before the first centre are identical (first map unchanged)
  a1 <- res$yearly[res$yearly$asset == "A1", ]
  expect_equal(length(unique(round(a1$aal_lp_revenue_rate, 14))), 1)

  # the zero-at-RP1 boundary (flood's convention) would understate heat AAL
  zero_rp1 <- calculate_aal(rps, loss)
  expect_gt(got, zero_rp1)

  # summary is the mean over the horizon
  expect_equal(res$summary$aal_lp_revenue_rate[res$summary$asset == "A1"], mean(a1$aal_lp_revenue_rate))
})

test_that("compute_global_heat_aal rises over time as later (hotter) maps take over", {
  fx <- make_heat_test_root()
  ev <- tibble::tibble(
    event_id = "ev1", hazard_type = "Heat", hazard_name = "h", scenario_name = "ssp585",
    statistic = "median", calc_mode = "probabilistic"
  )
  # last window centre is 2050.5, so 2051 and 2055 both use the last map unchanged
  res <- compute_global_heat_aal(heat_test_assets()[1, ], ev, fx$configs, fx$config_dir, c(2030, 2040, 2051, 2055))
  y <- res$yearly[order(res$yearly$year), ]
  expect_true(all(diff(y$aal_lp_revenue_rate) > 0 | abs(diff(y$aal_lp_revenue_rate)) < 1e-15))
  expect_gt(y$aal_lp_revenue_rate[[3]], y$aal_lp_revenue_rate[[1]])
  expect_equal(y$aal_lp_revenue_rate[[3]], y$aal_lp_revenue_rate[[4]]) # last map held beyond its centre
})

test_that("compute_aal_shock_trajectories adds the yearly heat rate to the revenue-rate channels", {
  baseline <- tibble::tibble(
    asset = "A", company = "C", year = 2030:2032, revenue = 1000, profit = 100
  )
  companies <- tibble::tibble(company = "C", net_profit_margin = 0.1)
  channels <- tibble::tibble(asset = "A", aal_pd_profit = 5, aal_pd_revenue_rate = 0.01, aal_bd_revenue_rate = 0.02)
  heat <- tibble::tibble(asset = "A", year = 2030:2032, aal_lp_revenue_rate = c(0.01, 0.02, 0.03))

  flood_only <- compute_aal_shock_trajectories(baseline, channels, companies)
  expect_equal(flood_only$revenue, rep(1000 * (1 - 0.03), 3))

  with_heat <- compute_aal_shock_trajectories(
    baseline, dplyr::mutate(channels, aal_lp_revenue_rate = 0.5), companies, aal_heat_yearly = heat
  )
  expect_equal(with_heat$revenue, 1000 * (1 - 0.03 - c(0.01, 0.02, 0.03)))
  expect_equal(with_heat$profit, with_heat$revenue * 0.1 - 5)

  # NULL heat leaves the flood-only numbers untouched even if the summary carries a heat column
  ignored <- compute_aal_shock_trajectories(baseline, dplyr::mutate(channels, aal_lp_revenue_rate = 0.5), companies)
  expect_equal(ignored$revenue, flood_only$revenue)
})

test_that("drop_heat_ui_inventory_columns removes only the statistic column", {
  inv <- tibble::tibble(hazard_type = "Heat", statistic = "median", return_period = 2L)
  expect_equal(names(drop_heat_ui_inventory_columns(inv)), c("hazard_type", "return_period"))
  expect_equal(drop_heat_ui_inventory_columns(tibble::tibble(a = 1)), tibble::tibble(a = 1))
  expect_null(drop_heat_ui_inventory_columns(NULL))
})

test_that("add_heat_revenue_adjustment = baseline revenue (event year) x output change", {
  af <- tibble::tibble(
    asset = c("A", "B", "C"), event_year = c(2035L, 2035L, 2035L),
    output_change_pct = c(-0.01, -0.02, NA_real_), hazard_type = c("Heat", "Heat", "Flood")
  )
  base <- tibble::tibble(
    asset = rep(c("A", "B", "C"), each = 2), year = rep(c(2034, 2035), 3), revenue = c(100, 110, 200, 220, 300, 330)
  )
  out <- add_heat_revenue_adjustment(af, base)
  expect_equal(out$baseline_revenue, c(110, 220, NA))
  expect_equal(out$revenue_adjustment, c(110 * -0.01, 220 * -0.02, NA))

  # flood-only tables (no output_change_pct) are returned untouched
  flood <- tibble::tibble(asset = "A", event_year = 2035L)
  expect_identical(add_heat_revenue_adjustment(flood, base), flood)
  expect_identical(add_heat_revenue_adjustment(NULL, base), NULL)
})

test_that("average annual revenue adjustment = mean over years of baseline revenue x (-AAL rate)", {
  heat_aal <- list(
    summary = tibble::tibble(asset = "A", aal_lp_revenue_rate = 0.02),
    yearly = tibble::tibble(asset = "A", year = 2030:2032, aal_lp_revenue_rate = c(0.01, 0.02, 0.03))
  )
  base <- tibble::tibble(asset = "A", year = 2030:2032, revenue = c(1000, 2000, 3000))
  out <- add_heat_aal_revenue_adjustment(heat_aal, base)

  expect_equal(out$yearly$revenue_adjustment, c(-10, -40, -90)) # negative = revenue lost
  expect_equal(out$summary$avg_annual_revenue_adjustment, mean(c(-10, -40, -90)))
  # consistent with the deterministic definition: baseline x output change, output change = -AAL rate
  expect_equal(out$yearly$revenue_adjustment, base$revenue * -heat_aal$yearly$aal_lp_revenue_rate)
})
