# Synthetic global administrative-region shapefiles (geoBoundaries layout) for the Heat
# spatial-separation tests. Aligned with the heat fixture assets (see helper-heat-global.R /
# make_heat_run_inputs): A1 (lon .5, lat .5) and A3 (lon -.5, lat 1.5) sit in country AAA's
# northern ADM1 "North"; A2 (lon 1.5, lat -.5) in country BBB.
#
#   AAA "Aland":  ADM1 North (lat 0..2, lon -2..1)    ADM2 "Twin" (lon -2..-1)  "Twin" (lon -1..1)
#                 ADM1 South (lat -2..0, lon -2..1)   ADM2 "Southwest" (lon -2..0) and an
#                                                          unnamed, self-intersecting (invalid) one (lon 0..1)
#   BBB "Bland":  ADM1 Only  (lon 1..2)               ADM2 "B-one" (lat -2..0)  "B-two" (lat 0..2)
#
# The two "Twin" ADM2 regions share both name and parent ADM1, like the many duplicate-named
# real municipalities.

make_adm_test_dir <- function(root = withr::local_tempdir(.local_envir = parent.frame())) {
  dir <- file.path(root, "spatial_separation", "global_adm_regions")
  box <- function(x0, y0, x1, y1) {
    sf::st_polygon(list(matrix(c(x0, y0, x1, y0, x1, y1, x0, y1, x0, y0), ncol = 2, byrow = TRUE)))
  }
  bowtie <- sf::st_polygon(list(matrix(c(0, -2, 1, 0, 1, -2, 0, 0, 0, -2), ncol = 2, byrow = TRUE)))

  write_level <- function(level, df, geoms) {
    d <- file.path(dir, sprintf("geoBoundariesCGAZ_ADM%d", level))
    dir.create(d, recursive = TRUE)
    layer <- sf::st_sf(df, geometry = sf::st_sfc(geoms, crs = 4326))
    suppressWarnings(sf::st_write(layer, file.path(d, sprintf("geoBoundariesCGAZ_ADM%d.shp", level)), quiet = TRUE))
  }

  write_level(0,
    data.frame(shapeGroup = c("BBB", "AAA"), shapeType = "ADM0", shapeName = c("Bland", "Aland")),
    list(box(1, -2, 2, 2), box(-2, -2, 1, 2))
  )
  write_level(1,
    data.frame(
      shapeName = c("North", "South", "Only"), shapeID = c("ID1N", "ID1S", "ID1O"),
      shapeGroup = c("AAA", "AAA", "BBB"), shapeType = "ADM1"
    ),
    list(box(-2, 0, 1, 2), box(-2, -2, 1, 0), box(1, -2, 2, 2))
  )
  write_level(2,
    data.frame(
      shapeName = c("Twin", "Twin", "Southwest", "", "B-one", "B-two"),
      shapeID = c("ID2T1", "ID2T2", "ID2SW", "ID2UN", "ID2B1", "ID2B2"),
      shapeGroup = c("AAA", "AAA", "AAA", "AAA", "BBB", "BBB"), shapeType = "ADM2"
    ),
    list(box(-2, 0, -1, 2), box(-1, 0, 1, 2), box(-2, -2, 0, 0), bowtie, box(1, -2, 2, 0), box(1, 0, 2, 2))
  )
  root
}

adm_test_assets <- function() {
  tibble::tibble(
    asset = c("in_north", "in_bland", "in_south", "nowhere", "no_coords"),
    company = "C",
    latitude = c(0.5, -0.5, -1, 50, NA),
    longitude = c(0.5, 1.5, -1.5, 50, NA)
  )
}
