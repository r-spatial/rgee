context("rgee: ee_extract test")
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
                           y = nc,
                           fun = ee$Reducer$max(),
                           sf = TRUE)
  expect_equal(mean(ee_nc_rain$X200012),53.29)
})

test_that("ee_extract ee$Image",{
  ee_nc_rain <- ee_extract(x = terraclimate$toBands(),
                           y = sf_as_ee(nc),
                           fun = ee$Reducer$max(),
                           sf = TRUE)
  expect_equal(mean(ee_nc_rain$X200012_pr),53.29)
})



test_that("ee_extract - error ",{
  demo <- environment()
  demo$name <- function() 1
  expect_error(ee_extract(x = demo, y = nc, sf = TRUE))
  expect_error(ee_extract(x = terraclimate, y = terraclimate, sf = TRUE))
  expect_error(ee_extract(x = terraclimate_raw, y = nc, sf = TRUE))
})
