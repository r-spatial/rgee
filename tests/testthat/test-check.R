context("rgee: ee_check test")
library(rgee)
ee_reattach()

test_that("simple ee_check ",{
  A <- ee_Initialize(
    email = 'data.colec.fbf@gmail.com',
    drive = TRUE,
    gcs = TRUE
  )
  expect_equal(A,TRUE)
})

