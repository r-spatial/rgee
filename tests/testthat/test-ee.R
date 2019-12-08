context("rgee: sf_as_ee test")

library(rgee)
library(reticulate)
library(raster)
library(sf)

ee_reattach()
ee_Initialize()
filename <- system.file("external/lux.shp", package="raster")

test_that("sf_as_ee.character",{
  p <- sf_as_ee(filename, check_ring_dir = TRUE)
  centroid <- p$
    geometry()$
    centroid()$
    getInfo() %>%
    '['('coordinates') %>%
    py_to_r() %>%
    mean()
  expect_equal(centroid,27.93429,tolerance=0.1)
})



test_that("sf_as_ee.sf",{
  p <- shapefile(filename) %>%
    st_as_sf() %>%
    sf_as_ee(check_ring_dir = TRUE)
  expect_is(p, "ee.featurecollection.FeatureCollection")
})

test_that("sf_as_ee.sfc",{
  p <- shapefile(filename) %>%
    st_as_sf() %>%
    st_geometry() %>%
    sf_as_ee(check_ring_dir = TRUE)
  expect_is(p, "ee.geometry.Geometry")
})

test_that("sf_as_ee.sfg",{
  p <- shapefile(filename) %>%
    st_as_sf() %>%
    st_geometry() %>%
    '[['(1) %>%
    sf_as_ee(check_ring_dir = TRUE)
  expect_is(p, "ee.geometry.Geometry")
})
