context("rgee: ee_print test")
skip_if_no_pypkg()
# -------------------------------------------------------------------------

# clean TRUE
test_that("simple ee_print test - ImageCollection", {
  eeobject <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
    filter(ee$Filter()$eq("WRS_PATH", 44))$
    filter(ee$Filter()$eq("WRS_ROW", 34))$
    filterDate("2014-03-01", "2014-08-01")
  ee_print_obj <- ee_print(
    eeobject = eeobject,
    clean = TRUE
  )
  ee_print_obj <- ee_print(eeobject = eeobject)
  expect_equal(ee_print_obj$name, "ImageCollection")
})

test_that("simple ee_print test -  2 - ImageCollection", {
  xmin <- -71.132591318
  xmax <- -70.953664315
  ymin <- -12.892451233
  ymax <- -12.731116372
  ROI <- c(xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin)
  ROI_polygon <- matrix(ROI, ncol = 2, byrow = TRUE) %>%
    list() %>%
    sf::st_polygon() %>%
    sf::st_sfc() %>%
    sf::st_set_crs(4326)
  ee_geom <- sf_as_ee(ROI_polygon)
  eeobject <- ee$ImageCollection("LANDSAT/LT05/C01/T1_SR")$
    filterBounds(ee$FeatureCollection(ee_geom))$
    filterDate("2011-01-01", "2011-12-31")
  ee_print_obj <- ee_print(
    eeobject = eeobject,
    clean = TRUE
  )
  ee_print_obj <- ee_print(eeobject = eeobject)
  expect_equal(ee_print_obj$name, "ImageCollection")
})

test_that("simple ee_print test - Image", {
  eeobject <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
    first()
  ee_print_obj <- ee_print(
    eeobject = eeobject,
    clean = TRUE
  )
  ee_print_obj <- ee_print(eeobject = eeobject)
  expect_equal(ee_print_obj$name, "Image")
})

test_that("simple ee_print test - FeatureCollection", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf")) %>%
    sf::st_transform(4326) %>%
    "["(1:10, )
  ee_nc <- sf_as_ee(nc)
  ee_print_obj <- ee_print(
    eeobject = ee_nc,
    clean = TRUE
  )
  ee_print_obj <- ee_print(eeobject = ee_nc)
  expect_equal(ee_print_obj$name, "FeatureCollection")
})

test_that("simple ee_print test - Feature", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf")) %>%
    sf::st_transform(4326) %>%
    "["(1, )
  ee_nc <- ee$Feature(sf_as_ee(nc)$first())
  ee_print_obj <- ee_print(
    eeobject = ee_nc,
    clean = TRUE
  )
  ee_print_obj <- ee_print(eeobject = ee_nc)
  expect_equal(ee_print_obj$name, "Feature")
})


test_that("simple ee_print test - Geometry", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf")) %>%
    sf::st_transform(4326) %>%
    "["(1, ) %>%
    sf::st_geometry()
  ee_nc <- sf_as_ee(nc)
  ee_print_obj <- ee_print(
    eeobject = ee_nc,
    clean = TRUE
  )
  ee_print_obj <- ee_print(eeobject = ee_nc)
  expect_equal(ee_print_obj$name, "Geometry")
})

# Testing create table
test_that("simple ee_print test - FeatureCollection", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf")) %>%
    sf::st_transform(4326) %>%
    "["(1:10, )
  ee_nc <- sf_as_ee(nc)
  ee_print_obj <- ee_print(
    eeobject = ee_nc,
    clean = TRUE
  )
  ee_print_obj <- ee_print(eeobject = ee_nc)
  expect_equal(ee_print_obj$name, "FeatureCollection")
})

# Testing print
test_that("different typ ee_print test - json", {
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf")) %>%
    sf::st_transform(4326) %>%
    "["(1, ) %>%
    sf::st_geometry()
  ee_nc <- sf_as_ee(nc)

  expect_equal(print(ee_nc, type = "json"), NULL)
  expect_equal(print(ee_nc, type = "simply"), NULL)
  ee_print_obj <- print(ee_nc, type = "ee_print")
  expect_equal(ee_print_obj$name, "Geometry")
})

test_that("simple ee_print test - ImageCollection not EPSG", {
mod11a2 <- ee$ImageCollection("MODIS/006/MOD11A2")$
  filter(ee$Filter$date('2001-01-01', '2002-12-31'))$
  filter(ee$Filter$calendarRange(7,field = "month"))
ee_print(eeobject = mod11a2)
})
