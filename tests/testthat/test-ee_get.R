context("rgee: ee_extract test")
skip_if_no_pypkg()
# -------------------------------------------------------------------------

test_that("ee_get images",{
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf")) %>%
    sf::st_transform(4326) %>%
    sf_as_ee()

  ee_s2 <- ee$ImageCollection("COPERNICUS/S2")$
    filterDate("2016-01-01", "2016-01-31")$
    filterBounds(nc)

  expect_equal(ee_get(ee_s2, index = 0:4)$size()$getInfo(), 5)
})

test_that("ee_get features",{
  fc <- ee$FeatureCollection(lapply(1:10, function(x) ee$Feature(NULL,list(index=x))))
  expect_equal(ee_get(fc, index = 0:4)$size()$getInfo(), 5)
})

test_that("ee_get error",{
  expect_error(ee_get(0:4))
})

test_that("ee_get error", {
  fc <- ee$FeatureCollection(lapply(1:10, function(x) ee$Feature(NULL,list(index=x))))
  expect_error(ee_get(fc, index = 4:0))
})
