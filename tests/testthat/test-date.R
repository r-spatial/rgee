context("rgee: ee_date test")

test_that("rdate_to_eedate I", {
  eedate <- rdate_to_eedate('2000-01-01')
  expect_true('ee.ee_date.Date' %in% class(eedate))
})

test_that("rdate_to_eedate II", {
  eedate <- rdate_to_eedate('2000-01-01',eeobject = FALSE)
  expect_equal(eedate, 946684800000)
})


test_that("eedate_to_rdate I", {
  ee_date <- ee$Date$fromYMD(2017, 2, 3)
  eedate <- eedate_to_rdate(ee_date)
  expect_s3_class(eedate, 'POSIXct')
})

test_that("eedate_to_rdate II", {
  ee_date <- ee$Date$fromYMD(2017, 2, 3)
  eedate <- eedate_to_rdate(ee_date,js = T)
  expect_equal(eedate, 1.48608e+12)
})

