context("rgee: ee_check test")

library(testthat)
library(rgee)
library(reticulate)
library(raster)
library(stars)
library(sf)

ee_Initialize()

filename <- system.file("external/lux.shp", package="raster")

test_that("simple ee_check ",{
  expect_null(ee_check())
})
