filename <- system.file("external/lux.shp", package="raster")
context("rgee: sf_as_ee test")

test_that("character",{
  p <- filename %>%
    sf_as_ee(check_ring_dir = TRUE)
  expect_is(p, "ee.featurecollection.FeatureCollection")
})

test_that("sf",{
  p <- shapefile(filename) %>%
    st_as_sf() %>%
    sf_as_ee(check_ring_dir = TRUE)
  expect_is(p, "ee.featurecollection.FeatureCollection")
})

test_that("sfc",{
  p <- shapefile(filename) %>%
    st_as_sf() %>%
    st_geometry() %>%
    sf_as_ee(check_ring_dir = TRUE)
  expect_is(p, "ee.geometry.Geometry")
})

test_that("sfg",{
  p <- shapefile(filename) %>%
    st_as_sf() %>%
    st_geometry() %>%
    '[['(1) %>%
    sf_as_ee(check_ring_dir = TRUE)
  expect_is(p, "ee.geometry.Geometry")
})
