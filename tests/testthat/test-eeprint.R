context("rgee: ee_print test")

test_that("simple ee_print test", {
  eeobject <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
    filter(ee$Filter()$eq("WRS_PATH", 44))$
    filter(ee$Filter()$eq("WRS_ROW", 34))$
    filterDate("2014-03-01", "2014-08-01")
  dd <- ee_print(eeobject, max_display = 3)
  expect_equal(dd,NULL)
})
