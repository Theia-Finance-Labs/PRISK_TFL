local_basin_cache <- function(env = parent.frame()) {
  # keep the on-disk polygon cache out of the user's real cache directory
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir(.local_envir = env), .local_envir = env)
  rm(list = ls(.global_basins_cache, all.names = TRUE), envir = .global_basins_cache)
}

test_that("load_global_basin_spatial_data reads attributes for both levels", {
  local_basin_cache()
  root <- make_basin_test_dir()
  sd <- load_global_basin_spatial_data(root)

  expect_equal(names(sd$global_basins$attributes), c("level2", "level4"))
  expect_equal(nrow(sd$global_basins$attributes$level2), 3)
  expect_equal(nrow(sd$global_basins$attributes$level4), 5)
  # ids are written in full, never in scientific notation
  expect_equal(sd$global_basins$attributes$level2$region_code, c("1020000010", "2020000010", "3020000010"))
  expect_true(all(c("region_code", "region_label", "macro_region") %in% names(sd$global_basins$attributes$level4)))

  expect_null(load_global_basin_spatial_data(file.path(root, "does-not-exist")))
  expect_null(load_global_basin_spatial_data(NULL))
})

test_that("macro regions and basin choices cascade by macro region", {
  local_basin_cache()
  sd <- load_global_basin_spatial_data(make_basin_test_dir())

  expect_equal(get_global_basin_macro_regions(sd, "level4"), c("Europe", "South America"))
  expect_equal(nrow(get_global_basin_choices(sd, "level4")), 5)
  expect_equal(nrow(get_global_basin_choices(sd, "level4", "Global")), 5) # "Global" lists every basin
  expect_equal(nrow(get_global_basin_choices(sd, "level4", "__all__")), 5)
  eu <- get_global_basin_choices(sd, "level4", "Europe")
  expect_equal(nrow(eu), 3)
  expect_equal(eu$region_label, sort(eu$region_label))
  expect_equal(nrow(get_global_basin_choices(sd, "level4", "Nowhere")), 0)
  expect_equal(nrow(get_global_basin_choices(sd, "level9")), 0)

  # same choices are available through the generic spatial helper
  gen <- get_spatial_region_choices(sd, "global_basins", "level2")
  expect_equal(names(gen), c("region_code", "region_label"))
  expect_equal(nrow(gen), 3)
  expect_equal(nrow(get_spatial_region_choices(sd, "global_basins", "global")), 0)
})

test_that("level choices for global_basins include the whole-area sentinel", {
  ch <- get_spatial_level_choices("global_basins")
  expect_equal(unname(ch), c("global", "level2", "level4"))
  # the Brazil schemes are untouched
  expect_equal(unname(get_spatial_level_choices("adm_regions")), c("brazil", "state", "municipality"))
  expect_equal(unname(get_spatial_level_choices("hydro_regions")), c("brazil", "macro", "meso", "micro"))
  expect_setequal(whole_area_spatial_levels(), c("brazil", "global"))
})

test_that("load_global_basin_layer repairs invalid polygons, and caches in memory and on disk", {
  local_basin_cache()
  root <- make_basin_test_dir()
  dir <- global_basins_dir(root)

  raw <- sf::st_read(global_basin_shp_path(dir, "level2"), quiet = TRUE)
  expect_false(all(suppressWarnings(sf::st_is_valid(raw)))) # the fixture really is invalid

  layer <- suppressMessages(load_global_basin_layer(dir, "level2"))
  expect_s3_class(layer, "sf")
  expect_true(all(sf::st_is_valid(layer)))
  expect_equal(names(layer), c("region_code", "region_label", "macro_region", "geometry"))
  expect_equal(nrow(layer), 3)

  # second call is served from the session cache (same object, no re-read)
  expect_silent(again <- load_global_basin_layer(dir, "level2"))
  expect_identical(again, layer)

  # a fresh session reads the on-disk cache instead of re-repairing
  rm(list = ls(.global_basins_cache, all.names = TRUE), envir = .global_basins_cache)
  expect_silent(from_disk <- load_global_basin_layer(dir, "level2"))
  expect_equal(from_disk$region_code, layer$region_code)

  expect_error(load_global_basin_layer(dir, "level9"), "Unknown global basin level")
})

test_that("evaluate_global_basin_selection includes only assets inside the selected basins", {
  local_basin_cache()
  sd <- load_global_basin_spatial_data(make_basin_test_dir())
  rows <- basin_test_assets()
  init <- function() {
    tibble::tibble(
      asset = rows$asset, spatial_included = FALSE, spatial_multiplier = 0,
      spatial_exposure_status = spatial_status_not_exposed()
    )
  }

  # level 4: select "North" (1040000010) -> only the asset at (5, 7)
  res <- suppressMessages(evaluate_global_basin_selection(rows, "level4", "1040000010", sd, init()))
  expect_equal(res$spatial_included, c(TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(res$spatial_multiplier, c(1, 0, 0, 0, 0))
  expect_true(is.na(res$spatial_exposure_status[[1]]))
  expect_equal(res$spatial_exposure_status[[2]], spatial_status_not_exposed())
  expect_equal(res$spatial_exposure_status[[4]], spatial_status_not_exposed()) # outside every basin
  expect_equal(res$spatial_exposure_status[[5]], spatial_status_insufficient()) # no coordinates

  # multi-select across macro regions
  res2 <- suppressMessages(evaluate_global_basin_selection(rows, "level4", c("2040000010", "3040000010"), sd, init()))
  expect_equal(res2$spatial_included, c(FALSE, TRUE, TRUE, FALSE, FALSE))

  # level 2: the first basin is a (repaired) bowtie, still selectable
  res3 <- suppressMessages(evaluate_global_basin_selection(rows, "level2", "2020000010", sd, init()))
  expect_equal(res3$spatial_included[[2]], TRUE)

  # unknown code -> nothing included
  res4 <- suppressMessages(evaluate_global_basin_selection(rows, "level4", "999", sd, init()))
  expect_false(any(res4$spatial_included))

  # no polygon data -> insufficient for everyone
  res5 <- evaluate_global_basin_selection(rows, "level4", "1040000010", list(), init())
  expect_true(all(res5$spatial_exposure_status == spatial_status_insufficient()))
})

test_that("evaluate_event_spatial_selection handles the whole-area sentinels for every scheme", {
  rows <- basin_test_assets()
  for (lvl in c("global", "brazil")) {
    res <- evaluate_event_spatial_selection(rows, "global_basins", lvl, character(0), character(0), NULL)
    expect_true(all(res$spatial_included))
    expect_equal(res$spatial_multiplier, rep(1, nrow(rows)))
  }
  # a basin level with nothing selected means "whole area", like Brazil's levels
  res <- evaluate_event_spatial_selection(rows, "global_basins", "level4", character(0), character(0), NULL)
  expect_true(all(res$spatial_included))
})

test_that("evaluate_spatial_separation resolves basin events by code and by label, leaving Brazil sentinels alone", {
  local_basin_cache()
  sd <- load_global_basin_spatial_data(make_basin_test_dir())
  rows <- basin_test_assets() |>
    tidyr::crossing(event_id = c("ev1", "ev2", "ev3", "ev4"))
  events <- tibble::tibble(
    event_id = c("ev1", "ev2", "ev3", "ev4"),
    hazard_type = "Flood",
    spatial_level = c("level4", "level4", "global", "brazil"),
    spatial_scheme = c("global_basins", "global_basins", "global_basins", NA),
    spatial_region_codes = c("1040000010", NA, NA, NA),
    # label contains a comma -> must not be split apart
    spatial_region_labels = c(NA, "North (with Rivers, Lakes)", NA, NA)
  )
  cfgs <- list(Flood = list(spatial_separation_scheme = "adm_regions"))

  res <- suppressMessages(evaluate_spatial_separation(rows, events, cfgs, spatial_separation_data = sd))
  inc <- function(ev) res$spatial_included[res$event_id == ev][order(res$asset[res$event_id == ev])]
  by_asset <- function(ev) stats::setNames(res$spatial_included[res$event_id == ev], res$asset[res$event_id == ev])

  expect_equal(unname(by_asset("ev1")[c("in_north", "in_east")]), c(TRUE, FALSE))
  expect_equal(by_asset("ev2"), by_asset("ev1")) # the label resolves to the same basin
  expect_true(all(by_asset("ev3"))) # whole area
  expect_true(all(by_asset("ev4"))) # legacy sentinel

  # whole-area events never trigger a boundary load (no spatial data, no base_dir needed)
  res_whole <- evaluate_spatial_separation(rows[rows$event_id == "ev3", ], events[3, ], cfgs, spatial_separation_data = NULL)
  expect_true(all(res_whole$spatial_included))
})

test_that("label parsing keeps commas inside basin names", {
  expect_equal(
    parse_spatial_values("North (with Rivers, Lakes)|South", split = SPATIAL_LABEL_SPLIT),
    c("North (with Rivers, Lakes)", "South")
  )
  expect_equal(parse_spatial_values("1;2|3,4"), c("1", "2", "3", "4")) # codes keep every delimiter
  expect_equal(
    parse_status_spatial_values("A (x, y)|B", split = "[|;]"),
    c("A (x, y)", "B")
  )
})

test_that("format_spatial_selection describes global basin selections", {
  expect_equal(format_spatial_selection("global", NA, NA, whole_area_label = "Global (whole)"), "Global (whole)")
  # a Global event is labelled "Global (whole)" even where no region-specific label is passed (results panels)
  expect_equal(format_spatial_selection("global", NA, NA), "Global (whole)")
  expect_equal(format_spatial_selection("brazil", NA, NA), "Brazil (whole)")
  expect_equal(
    format_spatial_selection("level2", "1", "Elbe (with Spree, Vltava)|Rhine"),
    "Basins (level 2): Elbe (with Spree, Vltava), Rhine"
  )
  out <- format_spatial_selection("level4", "1", "A|B|C|D|E")
  expect_equal(out, "Basins (level 4): A, B, C (+2 more)")
})

test_that("status helpers recognise the global_basins scheme", {
  expect_equal(infer_status_spatial_scheme("level2", NA), "global_basins")
  expect_equal(infer_status_spatial_scheme("level4", "global_basins"), "global_basins")
  expect_equal(infer_status_spatial_scheme("macro", NA), "hydro_regions")
  expect_equal(infer_status_spatial_scheme("state", NA), "adm_regions")
  expect_equal(get_hazard_spatial_scheme(list(Flood = list(spatial_separation_scheme = "global_basins")), "Flood"), "global_basins")
})

test_that("resolve_global_basin_event_geometry returns simplified selected polygons only", {
  local_basin_cache()
  sd <- load_global_basin_spatial_data(make_basin_test_dir())
  ev <- tibble::tibble(
    event_id = "ev7", spatial_level = "level4", spatial_region_codes = "1040000010|3040000020"
  )
  g <- suppressMessages(resolve_global_basin_event_geometry(ev, sd))
  expect_s3_class(g, "sf")
  expect_equal(sort(g$region_code), c("1040000010", "3040000020"))
  expect_true(all(g$event_id == "ev7"))
  expect_true(all(c("region_label", "macro_region") %in% names(g)))

  expect_null(resolve_global_basin_event_geometry(dplyr::mutate(ev, spatial_level = "global"), sd))
  expect_null(resolve_global_basin_event_geometry(dplyr::mutate(ev, spatial_region_codes = NA_character_), sd))
  expect_null(resolve_global_basin_event_geometry(dplyr::mutate(ev, spatial_region_codes = "000"), sd))
  expect_null(resolve_global_basin_event_geometry(ev, list()))
})

test_that("build_status_global_map draws selected basins and keeps asset markers", {
  local_basin_cache()
  sd <- load_global_basin_spatial_data(make_basin_test_dir())
  ev <- tibble::tibble(event_id = "ev1", spatial_level = "level4", spatial_region_codes = "1040000010")
  g <- suppressMessages(resolve_global_basin_event_geometry(ev, sd))
  assets <- tibble::tibble(asset = c("a", "b"), latitude = c(1, 2), longitude = c(3, 4))

  plain <- plotly::plotly_build(build_status_global_map(assets))
  with_basin <- plotly::plotly_build(build_status_global_map(assets, list(ev1 = g)))

  expect_gt(length(with_basin$x$data), length(plain$x$data))
  marker_traces <- Filter(function(d) identical(d$mode, "markers"), with_basin$x$data)
  expect_length(marker_traces, 1)
  expect_equal(as.numeric(marker_traces[[1]]$lon), c(3, 4)) # plotly adds an attribute to the vector

  # the view stays on the whole world instead of auto-zooming to the drawn polygons: plotly_build()
  # would otherwise write the polygons' bounding box into the geo axis ranges (regression)
  expect_false(isTRUE(with_basin$x$layout$geo$fitbounds))
  expect_equal(with_basin$x$layout$geo$scope, "world")
  expect_equal(as.numeric(with_basin$x$layout$geo$lataxis$range), c(-90, 90))
  expect_equal(as.numeric(with_basin$x$layout$geo$lonaxis$range), c(-180, 180))
  expect_equal(with_basin$x$layout$geo$lataxis, plain$x$layout$geo$lataxis)
  expect_equal(with_basin$x$layout$geo$lonaxis, plain$x$layout$geo$lonaxis)
  # ... and the polygon trace is still drawn
  expect_true(any(vapply(with_basin$x$data, function(d) identical(d$type, "scattergeo") && !identical(d$mode, "markers"), logical(1))))

  # still renders with neither assets nor basins
  expect_s3_class(build_status_global_map(NULL), "plotly")
})

test_that("as_multipolygon_layer normalises mixed geometry types, keeping one row per basin", {
  sq <- function(x0, y0, x1, y1) sf::st_polygon(list(matrix(c(x0, y0, x1, y0, x1, y1, x0, y1, x0, y0), ncol = 2, byrow = TRUE)))
  gc <- sf::st_geometrycollection(list(sq(0, 0, 1, 1), sf::st_point(c(5, 5)), sq(2, 2, 3, 3)))
  only_point_gc <- sf::st_geometrycollection(list(sf::st_point(c(1, 1))))
  x <- sf::st_sf(
    id = 1:4,
    geometry = sf::st_sfc(
      sq(0, 0, 1, 1), sf::st_multipolygon(list(sq(0, 0, 1, 1), sq(5, 5, 6, 6))), gc, only_point_gc,
      crs = 4326
    )
  )
  expect_gt(length(unique(as.character(sf::st_geometry_type(x)))), 1) # genuinely mixed

  out <- as_multipolygon_layer(x)
  expect_equal(nrow(out), 4)
  expect_equal(out$id, 1:4)
  expect_equal(unique(as.character(sf::st_geometry_type(out))), "MULTIPOLYGON")
  expect_equal(sf::st_crs(out), sf::st_crs(x))
  # the collection keeps both polygon parts; a collection with no polygon becomes empty, not an error
  expect_equal(length(out$geometry[[3]]), 2)
  expect_true(sf::st_is_empty(out)[[4]])
})

test_that("loaded basin layers have one geometry type so plotly can draw them", {
  local_basin_cache()
  sd <- load_global_basin_spatial_data(make_basin_test_dir())
  for (lv in c("level2", "level4")) {
    layer <- suppressMessages(get_global_basin_layer(sd, lv))
    expect_equal(unique(as.character(sf::st_geometry_type(layer))), "MULTIPOLYGON")
    expect_s3_class(sf::st_geometry(layer), "sfc_MULTIPOLYGON")
  }

  # regression: a mixed POLYGON/MULTIPOLYGON selection used to make the status map error
  # with "not implemented for objects of class sfc_GEOMETRY"
  ev <- tibble::tibble(event_id = "ev1", spatial_level = "level4", spatial_region_codes = "1040000010|2040000010|3040000020")
  g <- suppressMessages(resolve_global_basin_event_geometry(ev, sd))
  expect_s3_class(sf::st_geometry(g), "sfc_MULTIPOLYGON")
  expect_no_error(plotly::plotly_build(build_status_global_map(NULL, list(ev1 = g))))

  mixed <- g
  sf::st_geometry(mixed) <- sf::st_sfc(
    sf::st_cast(sf::st_geometry(g)[[1]], "POLYGON"), sf::st_geometry(g)[[2]], sf::st_geometry(g)[[3]], crs = 4326
  )
  expect_s3_class(sf::st_geometry(mixed), "sfc_GEOMETRY")
})
