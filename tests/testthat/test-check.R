context("rgee: ee_check test")

library(rgee)
library(reticulate)
library(raster)
library(stars)
library(sf)
ee <- reticulate::import("ee")
ee_Initialize(user_gmail = 'aybar1994@gmail.com',
              drive = TRUE,
              gcs = TRUE,
              checkpy = FALSE,
              assethome = 'users/aybar1994')

filename <- system.file("external/lux.shp", package="raster")

test_that("simple ee_check ",{
  expect_null(ee_check_rgee_python_packages())
  expect_null(ee_check_drivers())
  expect_null(ee_check_credentials())
})
