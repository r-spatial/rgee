context("rgee: sf_as_ee test")

if (isFALSE(exists('ee'))) {
  ee_reattach()
  ee_Initialize(
    email = 'data.colec.fbf@gmail.com',
    drive = TRUE,
    gcs = TRUE
  )
}

filename <- system.file("external/lux.shp", package = "raster")
nc <- system.file("shape/nc.shp", package = "sf")

test_that("sf_as_ee.character", {
  p <- sf_as_ee(filename, check_ring_dir = TRUE, geodesic  = TRUE)
  centroid <- p$
    geometry()$
    centroid()$
    getInfo() %>%
    "[["("coordinates") %>%
    ee_py_to_r() %>%
    mean()
  expect_equal(centroid, 27.93429, tolerance = 0.1)
})

test_that("sf_as_ee.sf", {
  p <- raster::shapefile(filename) %>%
    sf::st_as_sf()
  suppressWarnings(st_crs(p) <- 4326)
  p <- sf_as_ee(p, check_ring_dir = TRUE)
  expect_is(p, "ee.featurecollection.FeatureCollection")
})

test_that("sf_as_ee.sfc", {
  p <- raster::shapefile(filename) %>%
    sf::st_as_sf() %>%
    sf::st_geometry()
  suppressWarnings(st_crs(p) <- 4326)
  p <- sf_as_ee(p, check_ring_dir = TRUE)
  expect_is(p$geometry(), "ee.geometry.Geometry")
})

test_that("sf_as_ee.sfg", {
  p <- raster::shapefile(filename) %>%
    sf::st_as_sf() %>%
    st_geometry() %>%
    "[["(1) %>%
    sf_as_ee(check_ring_dir = TRUE)
  expect_is(p$geometry(), "ee.geometry.Geometry")
})

test_that("sf_as_ee - getInfo_to_asset", {
  remove_id <- "/users/datacolecfbf/sf_as_ee_test"
  try(ee_manage_delete(remove_id), silent = TRUE)
  p <- raster::shapefile(filename) %>%
    sf::st_as_sf() %>%
    st_geometry() %>%
    "[["(1) %>%
    sf_as_ee(
      check_ring_dir = TRUE,
      assetId = remove_id,
      via = "getInfo_to_asset")
   expect_is(p$geometry(), "ee.geometry.Geometry")
})

test_that("sf_as_ee - getInfo_to_asset", {
  remove_id <- "/users/datacolecfbf/sf_as_ee_test"
  try(ee_manage_delete(remove_id), silent = TRUE)
  p <- raster::shapefile(filename) %>%
    sf::st_as_sf() %>%
    st_geometry() %>%
    "[["(1) %>%
    sf_as_ee(
      check_ring_dir = TRUE,
      assetId = remove_id,
      bucket = "rgee_dev",
      via = "gcs_to_asset")
  expect_is(p$geometry(), "ee.geometry.Geometry")
})


test_that("sf_as_ee - getInfo_to_asset II", {
  remove_id <- "/users/datacolecfbf/sf_as_ee_test"
  try(ee_manage_delete(remove_id), silent = TRUE)
  p <- raster::shapefile(filename) %>%
    sf::st_as_sf() %>%
    st_geometry() %>%
    "[["(1) %>%
    sf_as_ee(
      check_ring_dir = TRUE,
      assetId = remove_id,
      bucket = "rgee_dev",
      monitoring = TRUE,
      via = "gcs_to_asset")
  expect_is(p$geometry(), "ee.geometry.Geometry")
})


test_that("ERROR 01", {
  expect_error(
    raster::shapefile(filename) %>%
      sf::st_as_sf() %>%
      sf::st_geometry() %>%
      sf_as_ee(check_ring_dir = TRUE,via = "cesar")
  )
})

test_that("ERROR 02", {
  ss_ff <- st_read(filename)
  st_crs(ss_ff) <- NA
  expect_error(
    sf_as_ee(ss_ff, check_ring_dir = TRUE, geodesic  = TRUE)
  )
})

