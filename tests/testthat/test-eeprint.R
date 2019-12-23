context("rgee: ee_print test")


# clean TRUE
test_that("simple ee_print test - ImageCollection", {
  eeobject <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
    filter(ee$Filter()$eq("WRS_PATH", 44))$
    filter(ee$Filter()$eq("WRS_ROW", 34))$
    filterDate("2014-03-01", "2014-08-01")
  ee_print_obj <- ee_print(eeobject = eeobject,
                           clean = TRUE,
                           max_display = 0)
  expect_equal(ee_print_obj$name,'ImageCollection')
})

test_that("simple ee_print test - Image", {
  eeobject <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
    first()
  ee_print_obj <- ee_print(eeobject = eeobject,
                           clean = TRUE,
                           max_display = 0)
  expect_equal(ee_print_obj$name,'Image')
})

test_that("simple ee_print test - FeatureCollection", {
  nc <- st_read(system.file("shape/nc.shp", package="sf")) %>%
    st_transform(4326) %>%
    '['(1:10,)
  ee_nc <- sf_as_ee(nc)
  ee_print_obj <- ee_print(eeobject = ee_nc,
                           clean = TRUE,
                           max_display = 0)
  expect_equal(ee_print_obj$name,'FeatureCollection')
})


test_that("simple ee_print test - Feature", {
  nc <- st_read(system.file("shape/nc.shp", package="sf")) %>%
    st_transform(4326) %>%
    '['(1,)
  ee_nc <- ee$Feature(sf_as_ee(nc)$first())
  ee_print_obj <- ee_print(eeobject = ee_nc,
                           clean = TRUE,
                           max_display = 0)
  expect_equal(ee_print_obj$name,'Feature')
})

test_that("simple ee_print test - Geometry", {
  nc <- st_read(system.file("shape/nc.shp", package="sf")) %>%
    st_transform(4326) %>%
    '['(1,) %>%
    st_geometry
  ee_nc <- sf_as_ee(nc)
  ee_print_obj <- ee_print(eeobject = ee_nc,
                           clean = TRUE,
                           max_display = 0)
  expect_equal(ee_print_obj$name,'Geometry')
})

# Testing create table
test_that("simple ee_print test - FeatureCollection", {
  nc <- st_read(system.file("shape/nc.shp", package="sf")) %>%
    st_transform(4326) %>%
    '['(1:10,)
  ee_nc <- sf_as_ee(nc)
  ee_print_obj <- ee_print(eeobject = ee_nc,
                           clean = TRUE,
                           max_display = 4)
  expect_equal(ee_print_obj$name,'FeatureCollection')
})

# Testing print
test_that("different typ ee_print test - json", {
  nc <- st_read(system.file("shape/nc.shp", package="sf")) %>%
    st_transform(4326) %>%
    '['(1,) %>%
    st_geometry
  ee_nc <- sf_as_ee(nc)

  expect_equal(print(ee_nc,type='json'),NULL)
  expect_equal(print(ee_nc,type='simply'),NULL)
  ee_print_obj <- print(ee_nc,type='ee_print')
  expect_equal(ee_print_obj$name,'Geometry')
})
