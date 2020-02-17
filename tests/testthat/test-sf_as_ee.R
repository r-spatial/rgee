context("rgee: sf_as_ee test")

ee_Initialize()
filename <- system.file("external/lux.shp", package = "raster")
nc <- system.file("shape/nc.shp", package = "sf")

test_that("sf_as_ee.character", {
  p <- sf_as_ee(filename, check_ring_dir = TRUE)
  centroid <- p$
    geometry()$
    centroid()$
    getInfo() %>%
    "["("coordinates") %>%
    ee_py_to_r() %>%
    mean()
  expect_equal(centroid, 27.93429, tolerance = 0.1)
  expect_error(sf_as_ee(nc))
})

test_that("sf_as_ee.sf", {
  p <- raster::shapefile(filename) %>%
    st_as_sf() %>%
    sf_as_ee(check_ring_dir = TRUE)
  expect_is(p, "ee.featurecollection.FeatureCollection")
  expect_error(sf_as_ee(st_read(nc)))
})

test_that("sf_as_ee.sfc", {
  p <- raster::shapefile(filename) %>%
    st_as_sf() %>%
    st_geometry() %>%
    sf_as_ee(check_ring_dir = TRUE)
  expect_is(p, "ee.geometry.Geometry")
  expect_error(sf_as_ee(st_read(nc)[["geometry"]]))
})

test_that("sf_as_ee.sfg", {
  p <- raster::shapefile(filename) %>%
    st_as_sf() %>%
    st_geometry() %>%
    "[["(1) %>%
    sf_as_ee(check_ring_dir = TRUE)

  expect_is(p, "ee.geometry.Geometry")
})
