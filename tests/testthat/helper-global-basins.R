# Synthetic global-basin shapefiles for the Global spatial-separation tests.
#
# Level 2 (3 basins) and level 4 (5 basins) laid out on a small grid. The first level-2
# polygon is deliberately a self-intersecting "bowtie" (invalid, like most real polygons
# in the shipped files), and one basin name contains a comma, as real names do.

make_basin_test_dir <- function(root = withr::local_tempdir(.local_envir = parent.frame())) {
  dir <- file.path(root, "spatial_separation", "global_basins", "hybas_global_named")
  dir.create(dir, recursive = TRUE)

  box <- function(x0, y0, x1, y1) {
    sf::st_polygon(list(matrix(c(x0, y0, x1, y0, x1, y1, x0, y1, x0, y0), ncol = 2, byrow = TRUE)))
  }
  bowtie <- sf::st_polygon(list(matrix(c(0, 0, 10, 10, 10, 0, 0, 10, 0, 0), ncol = 2, byrow = TRUE)))

  l2 <- sf::st_sf(
    HYBAS_ID = c(1020000010, 2020000010, 3020000010),
    MACRO_REG = c("Europe", "Europe", "South America"),
    BASIN_NAME = c("Alpha / Beta (with Gamma, Delta)", "Epsilon", "Zeta"),
    DESC = c("d1", "d2", "d3"),
    geometry = sf::st_sfc(bowtie, box(10, 0, 20, 10), box(-20, -10, -10, 0), crs = 4326)
  )
  l4 <- sf::st_sf(
    HYBAS_ID = c(1040000010, 1040000020, 2040000010, 3040000010, 3040000020),
    MACRO_REG = c("Europe", "Europe", "Europe", "South America", "South America"),
    BASIN_NAME = c("North (with Rivers, Lakes)", "South", "East", "Upper", "Lower"),
    DESC = c("a", "b", "c", "d", "e"),
    geometry = sf::st_sfc(
      box(0, 5, 10, 10), box(0, 0, 10, 5), box(10, 0, 20, 10), box(-20, -5, -10, 0), box(-20, -10, -10, -5),
      crs = 4326
    )
  )
  suppressWarnings({
    sf::st_write(l2, file.path(dir, "named_level2_global.shp"), quiet = TRUE)
    sf::st_write(l4, file.path(dir, "named_level4_global.shp"), quiet = TRUE)
  })
  root
}

basin_test_assets <- function() {
  tibble::tibble(
    asset = c("in_north", "in_east", "in_samerica", "nowhere", "no_coords"),
    company = "C",
    latitude = c(7, 5, -5, 50, NA),
    longitude = c(5, 15, -15, 50, NA)
  )
}
