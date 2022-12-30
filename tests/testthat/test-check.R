context("rgee: ee_check test")

library(sf)
library(rgee)

ee_Initialize(drive = TRUE, gcs = TRUE)

# -------------------------------------------------------------------------

test_that("simple ee_check ",{
  expect_true(ee_check_python())
  expect_true(ee_check_python_packages())
  expect_true(ee_check_credentials())
})

test_that("ee_check ",{
  expect_true(ee_check())
})


test_that("ee_wrong_message",{
  result <- rgee:::ee_wrong_message("sf")
  expect_null(result)
})


# test_that("ee_clean_credentials",{
#   result <- rgee:::ee_clean_credentials("user_demo")
#   expect_true(result)
# })


#test_that("ee_install check", {
  #message = ee_install(confirm = FALSE)
  #ee_clean_pyenv()
  #expect_true(message)
#})
