context("rgee: ee_check test")

library(rgee)
library(reticulate)
library(raster)
library(stars)
library(sf)

ee_Initialize(user_gmail = 'aybar1994@gmail.com')

filename <- system.file("external/lux.shp", package="raster")

test_that("simple ee_check ",{
  expect_null(ee_check_rgee_python_packages())
  expect_null(ee_check_drivers())
  expect_null(ee_check_credentials())

})
