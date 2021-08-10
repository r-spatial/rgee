context("rgee: ee_get test")
skip_if_no_pypkg()

library(rgee)
library(sf)

ee_Initialize()


blocks <- ee$FeatureCollection("TIGER/2010/Blocks")
roi <- ee$Geometry$Polygon(list(
  c(-122.275, 37.891),
  c(-122.275, 37.868),
  c(-122.240, 37.868),
  c(-122.240, 37.891)
))
subset <- blocks$filterBounds(roi)

test_that("ee_get fc", {
  nf <- rgee:::ee_get(subset)$size()$getInfo()
  expect_equal(nf, 1)

  nf <- rgee:::ee_get(subset, 0:4)$size()$getInfo()
  expect_equal(nf, 5)

  nf <- rgee:::ee_get(subset, c(3,2,4))$size()$getInfo()
  expect_equal(nf, 3)
})


nc <- st_read(system.file("shape/nc.shp", package = "sf")) %>%
  st_transform(4326) %>%
  sf_as_ee()

subset <- ee$ImageCollection("COPERNICUS/S2")$
  filterDate("2016-01-01", "2016-01-31")$
  filterBounds(nc)

test_that("ee_get ic", {
  nf <- rgee:::ee_get(subset)$size()$getInfo()
  expect_equal(nf, 1)

  nf <- rgee:::ee_get(subset, 0:4)$size()$getInfo()
  expect_equal(nf, 5)

  nf <- rgee:::ee_get(subset, c(3,2,4))$size()$getInfo()
  expect_equal(nf, 3)
})


test_that("ee_get error", {
  expect_error(rgee:::ee_get(ee$Image(0)))
})


test_that("ee_utils_dataset_display", {
  msg <- "AHN/AHN2_05M_INT"
  rs01 <- ee_utils_dataset_display(msg)
  img <- ee$Image("AHN/AHN2_05M_INT")
  rs02 <- ee_utils_dataset_display(img)
  expect_equal(rs01 + rs02, 2)
})

test_that("ee_utils_cog_metadata", {
  server <- "https://s3-us-west-2.amazonaws.com/planet-disaster-data/hurricane-harvey/"
  file <- "SkySat_Freeport_s03_20170831T162740Z3.tif"
  resource <- paste0(server, file)
  visParams <- list(nodata = 0, expression = "B3, B2, B1", rescale = "3000, 13500")
  msg1 <- ee_utils_cog_metadata(resource, visParams)
  testthat::expect_type(msg1, "list")
})
