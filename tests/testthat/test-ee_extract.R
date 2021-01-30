context("rgee: ee_extract test")
skip_if_no_pypkg()
# -------------------------------------------------------------------------
filename <- system.file("external/lux.shp", package="raster")

# data --------------------------------------------------------------------
terraclimate_raw <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")
terraclimate <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
  filterDate("2000-01-01", "2001-01-01")$
  map(function(x) x$select("pr"))
nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE) %>%
  st_transform(4326)

test_that("ee_extract ee$ImageCollection",{
  ee_nc_rain <- ee_extract(x = terraclimate,
                           y = nc["CRESS_ID"],
                           fun = ee$Reducer$max(),
                           sf = TRUE)
  expect_equal(mean(ee_nc_rain$X200012_pr), 53.29)
})

test_that("ee_extract ee$Image",{
  ee_nc_rain <- ee_extract(x = terraclimate$toBands(),
                           y = sf_as_ee(nc["CRESS_ID"]),
                           fun = ee$Reducer$max(),
                           sf = TRUE)
  expect_equal(mean(ee_nc_rain$X200012_pr), 53.29)
})

test_that("ee_extract sf = FALSE",{
  ee_nc_rain <- ee_extract(x = terraclimate$toBands(),
                           y = nc["CRESS_ID"],
                           fun = ee$Reducer$max(),
                           sf = FALSE)
  expect_equal(mean(ee_nc_rain$X200012_pr), 53.29)
})

test_that("ee_extract extra_test", {
  aoi <- ee$Geometry$Point(-115.2899, 48.2898)
  aoi_sf <- aoi
  collection <- ee$ImageCollection('UMT/NTSG/v2/LANDSAT/NPP') %>%
    ee$ImageCollection$filterBounds(aoi) %>%
    ee$ImageCollection$filterDate("2010-01-01", "2015-01-01") %>%
    ee$ImageCollection$select('annualNPP') %>%
    ee$ImageCollection$toBands()
  collection_tb <- ee_extract(
    x = collection,
    y = aoi_sf,
    fun = ee$Reducer$median(),
    scale = 1000,
    sf = FALSE
  )
  collection_tb_sf <- ee_extract(
    x = collection,
    y = aoi,
    fun = ee$Reducer$median(),
    scale = 1000,
    sf = TRUE
  )
  expect_equal(collection_tb$X2010_annualNPP, collection_tb_sf$X2010_annualNPP)
})

test_that("ee_extract - error ",{
  demo <- environment()
  demo$name <- function() 1
  expect_error(ee_extract(x = demo, y = nc, sf = TRUE))
  expect_error(ee_extract(x = terraclimate, y = terraclimate, sf = TRUE))
})

