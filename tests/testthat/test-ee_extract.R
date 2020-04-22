context("rgee: ee_extract test")

filename <- system.file("external/lux.shp", package="raster")

# data --------------------------------------------------------------------
terraclimate <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
  filterDate("2000-01-01", "2001-01-01")$
  map(function(x) x$select("pr"))
nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE) %>%
  st_transform(4326)

test_that("simple test ee_extract",{
  ee_nc_rain <- ee_extract(x = terraclimate,
                           y = nc,
                           fun = ee$Reducer$max(),
                           sf = TRUE)
  expect_equal(mean(ee_nc_rain$X200012),53.29)
})
