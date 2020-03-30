context("rgee: ee_check test")

ee_reattach()
ee_Initialize(email = 'data.colec.fbf@gmail.com',
              drive = TRUE,
              gcs = TRUE)
filename <- system.file("external/lux.shp", package = "raster")

test_that("simple ee_check ",{
  expect_true(ee_check_python())
  expect_null(ee_check_rgee_python_packages())
  expect_null(ee_check_credentials())
})

test_that("ee_check ",{
  expect_null(ee_check())
})
