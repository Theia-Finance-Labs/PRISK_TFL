# Global administrative-region separation (Heat): attributes, choices, selection semantics,
# polygons and point-in-polygon matching, on the synthetic fixture (see helper-global-adm.R).

adm_data <- function(env = parent.frame()) {
  root <- make_adm_test_dir(withr::local_tempdir(.local_envir = env))
  list(root = root, data = load_global_adm_spatial_data(root))
}

adm_pre_res <- function(assets) {
  tibble::tibble(
    asset = assets$asset, spatial_included = FALSE, spatial_multiplier = 0,
    spatial_exposure_status = spatial_status_not_exposed()
  )
}

test_that("attributes load for the three levels, with unnamed regions labelled", {
  fx <- adm_data()
  a <- fx$data$global_adm$attributes
  expect_equal(vapply(a, nrow, integer(1)), c(adm0 = 2L, adm1 = 3L, adm2 = 6L))
  expect_setequal(a$adm0$region_code, c("AAA", "BBB"))
  expect_setequal(a$adm1$region_code, c("ID1N", "ID1S", "ID1O"))
  expect_equal(a$adm2$name[a$adm2$region_code == "ID2UN"], GLOBAL_ADM_UNNAMED_LABEL)
  expect_equal(a$adm2$country_code[a$adm2$region_code == "ID2B1"], "BBB")

  expect_null(load_global_adm_spatial_data(withr::local_tempdir()))
  expect_null(load_global_adm_spatial_data(NULL))
})

test_that("the global loader combines basins and administrative regions", {
  root <- make_adm_test_dir(withr::local_tempdir())
  make_basin_test_dir(root)
  both <- load_global_spatial_data(root)
  expect_true(all(c("global_basins", "global_adm") %in% names(both)))
  expect_null(load_global_spatial_data(withr::local_tempdir()))

  only_adm <- load_global_spatial_data(make_adm_test_dir(withr::local_tempdir()))
  expect_true("global_adm" %in% names(only_adm))
  expect_false("global_basins" %in% names(only_adm))
})

test_that("level choices and scheme plumbing know the administrative levels", {
  lv <- get_spatial_level_choices("global_adm")
  expect_equal(unname(lv), c("global", "adm0", "adm1", "adm2"))
  expect_equal(infer_status_spatial_scheme("adm1", NA), "global_adm")
  expect_equal(infer_status_spatial_scheme("level2", NA), "global_basins")
  expect_equal(infer_status_spatial_scheme("state", NA), "adm_regions")
  expect_equal(infer_status_spatial_scheme("adm0", "global_adm"), "global_adm")
  expect_equal(format_spatial_selection("adm0", "AAA|BBB", "Aland|Bland", "Global (whole)"), "Countries (ADM0): Aland, Bland")
  expect_equal(format_spatial_selection("adm2", "x", "Twin in North (AAA)", "Global (whole)"), "ADM2 regions: Twin in North (AAA)")
  expect_equal(format_spatial_selection("global", NA, NA, "Global (whole)"), "Global (whole)")
})

test_that("country choices are sorted by name and keyed by ISO3", {
  fx <- adm_data()
  ch <- get_global_adm_country_choices(fx$data)
  expect_equal(ch$region_code, c("AAA", "BBB"))
  expect_equal(ch$region_label, c("Aland", "Bland"))
})

test_that("region choices are scoped to the chosen countries and labelled unambiguously", {
  fx <- adm_data()
  expect_equal(get_global_adm_region_choices(fx$data, "adm0")$region_label, c("Aland", "Bland"))
  expect_equal(get_global_adm_region_choices(fx$data, "adm0", "BBB")$region_label, "Bland")

  a1 <- get_global_adm_region_choices(fx$data, "adm1", "AAA")
  expect_equal(a1$region_label, c("North (AAA)", "South (AAA)"))
  expect_equal(nrow(get_global_adm_region_choices(fx$data, "adm1")), 3)

  # ADM2 is only listed for explicit countries; labels carry the parent ADM1 and the country
  expect_equal(nrow(get_global_adm_region_choices(fx$data, "adm2")), 0)
  a2 <- get_global_adm_region_choices(fx$data, "adm2", "AAA")
  expect_equal(nrow(a2), 4)
  expect_equal(a2$region_label[a2$region_code == "ID2SW"], "Southwest in South (AAA)")
  # the two identically named regions with the same parent stay distinguishable
  twins <- a2$region_label[a2$region_code %in% c("ID2T1", "ID2T2")]
  expect_equal(length(unique(twins)), 2)
  expect_true(all(grepl("^Twin in North \\(AAA\\) \\[[12]\\]$", twins)))
  expect_false(anyDuplicated(a2$region_label) > 0)
  # labels never contain the characters used to split stored label lists
  expect_false(any(grepl("[|;,]", a2$region_label)))
  expect_equal(a2$region_label[a2$region_code == "ID2UN"], "(unnamed) in South (AAA)")
  expect_equal(get_global_adm_region_choices(fx$data, "adm2", "BBB")$region_label, c("B-one in Only (BBB)", "B-two in Only (BBB)"))
})

test_that("build_global_adm_selection resolves level, countries and regions", {
  fx <- adm_data()
  d <- fx$data

  s0 <- build_global_adm_selection("adm0", c("AAA", "BBB"), character(0), d)
  expect_equal(s0$level, "adm0")
  expect_equal(s0$codes, c("AAA", "BBB"))
  expect_equal(s0$labels, c("Aland", "Bland"))

  # regions chosen -> exactly those regions
  s1 <- build_global_adm_selection("adm1", "AAA", "ID1N", d)
  expect_equal(c(s1$level, s1$codes, s1$labels), c("adm1", "ID1N", "North (AAA)"))
  s2 <- build_global_adm_selection("adm2", "AAA", c("ID2SW", "ID2T1"), d)
  expect_equal(s2$level, "adm2")
  expect_equal(s2$codes, c("ID2SW", "ID2T1"))
  expect_equal(length(s2$labels), 2)
  expect_false(anyNA(s2$labels))

  # countries without regions -> the whole countries, stored as ADM0
  s3 <- build_global_adm_selection("adm1", "BBB", character(0), d)
  expect_equal(c(s3$level, s3$codes, s3$labels), c("adm0", "BBB", "Bland"))

  # nothing chosen / unknown level -> whole area
  for (s in list(
    build_global_adm_selection("adm1", character(0), character(0), d),
    build_global_adm_selection("adm0", NULL, NULL, d),
    build_global_adm_selection("global", "AAA", "ID1N", d),
    build_global_adm_selection("bogus", "AAA", NULL, d),
    build_global_adm_selection(NA, NULL, NULL, d)
  )) {
    expect_equal(s$level, GLOBAL_WHOLE_AREA_LEVEL)
    expect_length(s$codes, 0)
  }
})

test_that("polygons are read per country, repaired, MULTIPOLYGON and cached", {
  fx <- adm_data()
  d <- fx$data

  aaa <- read_global_adm_polygons(d, "adm2", c("ID2T1", "ID2UN"))
  expect_s3_class(aaa, "sf")
  expect_setequal(aaa$region_code, c("ID2T1", "ID2UN"))
  expect_true(all(as.character(sf::st_geometry_type(aaa)) == "MULTIPOLYGON"))
  # the shipped bowtie is invalid; the loaded layer is not
  expect_true(all(suppressWarnings(sf::st_is_valid(aaa))))
  expect_equal(aaa$name[aaa$region_code == "ID2UN"], GLOBAL_ADM_UNNAMED_LABEL)

  whole <- read_global_adm_polygons(d, "adm1", c("AAA", "BBB"), whole_countries = TRUE)
  expect_equal(nrow(whole), 3)
  expect_equal(nrow(read_global_adm_polygons(d, "adm0", "BBB")), 1)

  expect_null(read_global_adm_polygons(d, "adm1", character(0)))
  expect_null(read_global_adm_polygons(d, "adm1", "no-such-id"))
  # codes that look like SQL are never interpolated into the query
  expect_null(read_global_adm_polygons(d, "adm1", "x' OR '1'='1", whole_countries = TRUE))
  expect_identical(read_global_adm_polygons(d, "adm2", "ID2T1"), read_global_adm_polygons(d, "adm2", "ID2T1"))
})

test_that("point-in-polygon matching includes exactly the assets inside the selection", {
  fx <- adm_data()
  d <- fx$data
  assets <- adm_test_assets()
  included <- function(level, codes) {
    r <- evaluate_global_adm_selection(assets, level, codes, d, adm_pre_res(assets))
    r$asset[r$spatial_included]
  }

  expect_equal(included("adm0", "AAA"), c("in_north", "in_south"))
  expect_equal(included("adm0", "BBB"), "in_bland")
  expect_equal(included("adm0", c("AAA", "BBB")), c("in_north", "in_bland", "in_south"))
  expect_equal(included("adm1", "ID1N"), "in_north")
  expect_equal(included("adm1", c("ID1N", "ID1O")), c("in_north", "in_bland"))
  expect_equal(included("adm2", "ID2T2"), "in_north")
  expect_equal(included("adm2", "ID2T1"), character(0))
  expect_equal(included("adm2", c("ID2SW", "ID2B1")), c("in_bland", "in_south"))
  expect_equal(included("adm1", "does-not-exist"), character(0))

  r <- evaluate_global_adm_selection(assets, "adm0", "AAA", d, adm_pre_res(assets))
  expect_equal(r$spatial_multiplier[r$asset == "in_north"], 1)
  expect_true(is.na(r$spatial_exposure_status[r$asset == "in_north"]))
  expect_equal(r$spatial_exposure_status[r$asset == "in_bland"], spatial_status_not_exposed())
  expect_equal(r$spatial_exposure_status[r$asset == "no_coords"], spatial_status_insufficient())
  expect_false(r$spatial_included[r$asset == "no_coords"])

  # no administrative data loaded -> nothing can be placed
  none <- evaluate_global_adm_selection(assets, "adm0", "AAA", list(), adm_pre_res(assets))
  expect_false(any(none$spatial_included))
  expect_true(all(none$spatial_exposure_status == spatial_status_insufficient()))
})

test_that("evaluate_spatial_separation routes global_adm events and loads the data lazily", {
  fx <- adm_data()
  assets <- adm_test_assets()
  awe <- assets |> dplyr::mutate(event_id = "ev1")
  cfg <- list(Heat = list(spatial_separation_scheme = "global_adm"))
  ev <- tibble::tibble(
    event_id = "ev1", hazard_type = "Heat", spatial_level = "adm1", spatial_region_codes = "ID1N",
    spatial_region_labels = NA_character_, spatial_scheme = "global_adm"
  )

  with_data <- evaluate_spatial_separation(awe, ev, cfg, spatial_separation_data = fx$data, base_dir = fx$root)
  lazy <- evaluate_spatial_separation(awe, ev, cfg, spatial_separation_data = NULL, base_dir = fx$root)
  expect_equal(with_data$asset[with_data$spatial_included], "in_north")
  expect_equal(lazy, with_data)

  # the scheme can also come from the hazard config when the event row carries none
  ev_cfg <- ev |> dplyr::mutate(spatial_scheme = NA_character_)
  expect_equal(
    evaluate_spatial_separation(awe, ev_cfg, cfg, spatial_separation_data = fx$data)$spatial_included,
    with_data$spatial_included
  )

  # a whole-area event includes everything, without touching any polygons
  ev_whole <- ev |> dplyr::mutate(spatial_level = "global", spatial_region_codes = NA_character_)
  whole <- evaluate_spatial_separation(awe, ev_whole, cfg, spatial_separation_data = NULL, base_dir = NULL)
  expect_true(all(whole$spatial_included))

  # codes missing -> resolved through labels (ADM0 labels are country names)
  ev_lab <- ev |> dplyr::mutate(spatial_level = "adm0", spatial_region_codes = NA_character_, spatial_region_labels = "Bland")
  lab <- evaluate_spatial_separation(awe, ev_lab, cfg, spatial_separation_data = fx$data)
  expect_equal(lab$asset[lab$spatial_included], "in_bland")
})

test_that("status-map geometry: one feature per region, dissolved per country for large selections", {
  fx <- adm_data()
  d <- fx$data
  ev <- tibble::tibble(event_id = "ev7", spatial_level = "adm2", spatial_region_codes = "ID2T1|ID2T2|ID2SW|ID2B1")

  g <- resolve_global_adm_event_geometry(ev, d)
  expect_s3_class(g, "sf")
  expect_equal(nrow(g), 4)
  expect_setequal(names(g), c("event_id", "region_code", "region_label", "geometry"))
  expect_equal(unique(g$event_id), "ev7")
  expect_true(all(as.character(sf::st_geometry_type(g)) == "MULTIPOLYGON"))
  expect_setequal(g$region_label, c("Twin", "Twin", "Southwest", "B-one"))

  # more regions than max_regions -> one dissolved feature per country, named with the count
  dg <- resolve_global_adm_event_geometry(ev, d, max_regions = 2)
  expect_equal(nrow(dg), 2)
  expect_setequal(dg$region_code, c("AAA", "BBB"))
  expect_equal(dg$region_label[dg$region_code == "AAA"], "Aland: 3 regions")
  expect_equal(dg$region_label[dg$region_code == "BBB"], "Bland: 1 regions")

  # ADM0 selection, and the dispatcher also serves basin events
  c0 <- resolve_global_event_geometry(
    tibble::tibble(event_id = "ev1", spatial_level = "adm0", spatial_region_codes = "AAA"), d
  )
  expect_equal(c0$region_label, "Aland")

  expect_null(resolve_global_adm_event_geometry(tibble::tibble(event_id = "e", spatial_level = "adm1", spatial_region_codes = NA_character_), d))
  expect_null(resolve_global_adm_event_geometry(tibble::tibble(event_id = "e", spatial_level = "level2", spatial_region_codes = "x"), d))
  expect_null(resolve_global_event_geometry(tibble::tibble(event_id = "e", spatial_level = "global", spatial_region_codes = NA_character_), d))
  expect_null(resolve_global_event_geometry(tibble::tibble(event_id = "e", spatial_level = "state", spatial_region_codes = "x"), d))

  both <- load_global_spatial_data({
    root <- make_adm_test_dir(withr::local_tempdir())
    make_basin_test_dir(root)
  })
  basin <- resolve_global_event_geometry(
    tibble::tibble(event_id = "evb", spatial_level = "level2", spatial_region_codes = "2020000010"), both
  )
  expect_equal(basin$region_label, "Epsilon")
})

test_that("the hazard-config validation accepts the global_adm scheme", {
  expect_equal(get_hazard_spatial_scheme(list(Heat = list(spatial_separation_scheme = "global_adm")), "Heat"), "global_adm")
  expect_equal(get_hazard_spatial_scheme(list(Heat = list(spatial_separation_scheme = "nonsense")), "Heat"), "adm_regions")
})

test_that("the Global map keeps the whole-world view with administrative regions drawn", {
  fx <- adm_data()
  ev <- tibble::tibble(event_id = "ev1", spatial_level = "adm0", spatial_region_codes = "BBB")
  g <- resolve_global_adm_event_geometry(ev, fx$data)
  plain <- plotly::plotly_build(build_status_global_map(NULL))
  drawn <- plotly::plotly_build(build_status_global_map(NULL, list(ev1 = g)))
  filled <- function(b) Filter(function(d) identical(d$fill, "toself"), b$x$data)
  expect_length(filled(plain), 0)
  expect_length(filled(drawn), 1)
  expect_true(all(grepl("Bland", filled(drawn)[[1]]$text)))
  expect_equal(as.numeric(drawn$x$layout$geo$lataxis$range), c(-90, 90))
  expect_equal(as.numeric(drawn$x$layout$geo$lonaxis$range), c(-180, 180))
})
