context("rgee: ee_check test")
skip_if_no_pypkg()
# -------------------------------------------------------------------------

test_that("simple ee_check ",{
  expect_true(ee_check_python())
  expect_true(ee_check_python_packages())
  expect_true(ee_check_credentials())
})

test_that("ee_check ",{
  expect_null(ee_check())
})

