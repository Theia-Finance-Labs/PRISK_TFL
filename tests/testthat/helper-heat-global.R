# Synthetic fixtures for the Global heat (labour-productivity) engine.
#
# Builds a tiny data root:
#   <root>/hazards/config_global/Heat.yml
#   <root>/hazards/indicators/heat_maps/hi35_change_<ssp>_<start>-<end>.tif   (29 bands)
#   <root>/hazards/mappings/Nace_exposure.xlsx
#   <root>/hazards/mappings/CB_function_data_per_country.xlsx   (country name / ISO identifiers)
#   <root>/hazards/mappings/IMF_classification.xlsx             (country NAMES -> IMF group)
#   <root>/hazards/mappings/imf_country_aliases.csv             (explicit name -> ISO crosswalk rows)
#
# Map values are deterministic so blended results can be computed by hand:
#   excess_<stat>_RP<n>  =  base_stat + 10 * window_index + rp_index   (window_index 1..3)
# with base_stat = 0 (median), 100 (mean), -50 (p10), 50 (p90).

heat_test_windows <- function() {
  data.frame(
    start = c(2021L, 2031L, 2041L),
    end = c(2040L, 2050L, 2060L),
    centre = c(2030.5, 2040.5, 2050.5)
  )
}

heat_test_rps <- function() c(2, 5, 10, 25, 50, 100)

heat_test_value <- function(stat, window_index, rp) {
  base <- c(median = 0, mean = 100, p10 = -50, p90 = 50)[[stat]]
  base + 10 * window_index + match(rp, heat_test_rps())
}

make_heat_test_maps <- function(maps_dir, ssps = c("ssp585", "ssp126")) {
  dir.create(maps_dir, recursive = TRUE, showWarnings = FALSE)
  wins <- heat_test_windows()
  rps <- heat_test_rps()
  stats <- c("median", "mean", "p10", "p90")
  band_grid <- expand.grid(rp = rps, stat = stats, stringsAsFactors = FALSE)
  band_names <- c(
    paste0("excess_", band_grid$stat, "_RP", band_grid$rp),
    "baseline_median", "baseline_mean", "baseline_p10", "baseline_p90", "n_models"
  )
  for (ssp in ssps) {
    ssp_offset <- if (identical(ssp, "ssp126")) -1 else 0
    for (w in seq_len(nrow(wins))) {
      layers <- lapply(seq_len(nrow(band_grid)), function(i) {
        r <- terra::rast(nrows = 4, ncols = 4, xmin = -2, xmax = 2, ymin = -2, ymax = 2, crs = "EPSG:4326")
        terra::values(r) <- heat_test_value(band_grid$stat[[i]], w, band_grid$rp[[i]]) + ssp_offset
        r
      })
      extra <- lapply(1:5, function(i) {
        r <- terra::rast(nrows = 4, ncols = 4, xmin = -2, xmax = 2, ymin = -2, ymax = 2, crs = "EPSG:4326")
        terra::values(r) <- i
        r
      })
      stack <- do.call(c, c(layers, extra))
      names(stack) <- band_names
      terra::writeRaster(
        stack,
        file.path(maps_dir, sprintf("hi35_change_%s_%d-%d.tif", ssp, wins$start[[w]], wins$end[[w]])),
        overwrite = TRUE,
        datatype = "FLT4S" # p10 values are negative; integer storage would wrap them
      )
    }
  }
  invisible(maps_dir)
}

heat_test_nace_table <- function() {
  data.frame(
    `NACE Section` = c("A", "A", "A", "A", "C", "C", "C", "C", "K"),
    Code = c(NA, "1", "1.1", "1.1.1", NA, "10", "10.1", "10.1.1", "64"),
    Description = c(
      "Agriculture, forestry and fishing", "Crop and animal production", "Growing of non-perennial crops",
      "Growing of cereals", "Manufacturing", "Manufacture of food products", "Processing of meat",
      "Processing and preserving of meat", "Financial service activities"
    ),
    Level = c("Section", "Division", "Group", "Class", "Section", "Division", "Group", "Class", "Division"),
    `Overall Exposure` = c("High", "High", "High", "High", "Medium", "Medium", "Low", "Low", "Low"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

heat_test_cb_table <- function() {
  data.frame(
    period = "2023",
    countryRegionIso2 = c("AA", "BB", "CC", "DD", "Global-AVG"),
    countryRegionId = c("AAA", "BBB", "CCC", "DDD", "Global-AVG"),
    countryRegionName = c("Alphaland", "Betaland", "Gammaland", "Deltaland", "Global-AVG"),
    E = c("10", "20", "30", "40", "5"),
    Unit = "QBTU",
    emp = c("100", "200", "300", "400", "50"),
    hc = c("2", "3", "", "4", "2"),
    rnna = c("1000", "2000", "3000", "4000", "500"),
    hc_source = "test",
    stringsAsFactors = FALSE
  )
}

heat_test_config_yaml <- function(beta_advanced = 0.535, beta_emde = 0.399) {
  c(
    "indicators:",
    "  heat_excess_days:",
    "    file: heat_maps/",
    "    type: global_heat_maps",
    "    variable: excess_days",
    "    index: [scenario_name, return_period, statistic]",
    "    agg: mean",
    "index_indicator: heat_excess_days",
    "spatial_separation_scheme: adm_regions",
    "mappings:",
    "  nace_exposure:",
    "    file: Nace_exposure.xlsx",
    "    variables: [overall_exposure]",
    "    join:",
    "      on_assets: [nace_code]",
    "shocks:",
    "  revenue:",
    "    equations:",
    "      - name: heat_labor_productivity_global",
    "        equation: |",
    "          revenue ~ revenue * (1 + output_change_pct)",
    "        constants:",
    paste0("          beta_L_advanced: ", beta_advanced),
    paste0("          beta_L_emde: ", beta_emde)
  )
}

#' IMF classification fixture: names only (like the real file)
heat_test_imf_table <- function() {
  data.frame(
    Country = c("Alphaland", "Betaland", "Gammaland", "The Delta Republic", "Zetaland"),
    Classification = c(
      "Advanced Economies", "Emerging Market and Developing Economies", "Advanced Economies",
      "Emerging Market and Developing Economies", "Advanced Economies"
    ),
    stringsAsFactors = FALSE
  )
}

#' Alias crosswalk fixture: the IMF spelling differs from the code table ("The Delta Republic")
heat_test_imf_aliases <- function() {
  data.frame(
    imf_name = c("The Delta Republic", "Zetaland"),
    iso2 = c("DD", "ZE"), iso3 = c("DDD", "ZET"),
    basis = c("spelling alias of 'Deltaland'", "standard code; not in country table"),
    stringsAsFactors = FALSE
  )
}

#' Build the synthetic data root and return its paths and loaded configs
make_heat_test_root <- function(root = withr::local_tempdir(.local_envir = parent.frame())) {
  config_dir <- file.path(root, "hazards", "config_global")
  indicators_dir <- file.path(root, "hazards", "indicators")
  mappings_dir <- file.path(root, "hazards", "mappings")
  dir.create(config_dir, recursive = TRUE)
  dir.create(mappings_dir, recursive = TRUE)
  writeLines(heat_test_config_yaml(), file.path(config_dir, "Heat.yml"))
  make_heat_test_maps(file.path(indicators_dir, "heat_maps"))
  writexl::write_xlsx(heat_test_nace_table(), file.path(mappings_dir, "Nace_exposure.xlsx"))
  writexl::write_xlsx(heat_test_cb_table(), file.path(mappings_dir, "CB_function_data_per_country.xlsx"))
  writexl::write_xlsx(heat_test_imf_table(), file.path(mappings_dir, "IMF_classification.xlsx"))
  utils::write.csv(heat_test_imf_aliases(), file.path(mappings_dir, "imf_country_aliases.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  list(
    root = root,
    config_dir = config_dir,
    indicators_dir = indicators_dir,
    mappings_dir = mappings_dir,
    maps_dir = file.path(indicators_dir, "heat_maps"),
    configs = load_hazard_configs(config_dir)
  )
}

heat_test_assets <- function() {
  tibble::tibble(
    asset = c("A1", "A2", "A3", "A4"),
    company = c("C1", "C1", "C2", "C2"),
    latitude = c(0.5, -0.5, NA, 10),
    longitude = c(0.5, 1.5, 0.5, 0.5),
    country = c("AA", "BBB", "ZZ", "AA"),
    sector = c("1.1000000000000001", "10.1.1", "unknown thing", "64"),
    asset_category = "commercial building",
    share_of_economic_activity = 1
  )
}
