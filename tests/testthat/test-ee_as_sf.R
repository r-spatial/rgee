context("rgee: ee_as_sf test")

# -------------------------------------------------------------------------

# Load and filter watersheds from a data table.
sheds <- ee$FeatureCollection('USGS/WBD/2017/HUC06')$
  filterBounds(ee$Geometry$Rectangle(-127.18, 19.39, -62.75, 51.29))$
  map(function(feature) {
    num <- ee$Number$parse(feature$get('areasqkm'))
    return(feature$set('areasqkm', num))
})

region <- ee$Geometry$Rectangle(-119.224, 34.669, -99.536, 50.064)
ee_randomPoints <- ee$FeatureCollection$randomPoints(region, 15000)


test_that("sf small ",{
  mysheds <- ee_as_sf(ee$Feature(sheds$first()))
  expect_equal(mysheds$areaacres, "1064898.31")
  expect_error(ee_as_sf(sheds$first()))
})

test_that("sf large",{
  sf_large <- ee_as_sf(ee_randomPoints, maxFeatures = 15000)
  expect_s3_class(sf_large,"sf")
})


test_that("sf - drive",{
  mysheds <- ee_as_sf(ee$Feature(sheds$first()),via = "drive")
  expect_s3_class(mysheds,"sf")
})

test_that("sf - gcs",{
  mysheds <- ee_as_sf(ee$Feature(sheds$first()),via = "gcs",
                      container = "rgeedev2")
  expect_s3_class(mysheds,"sf")
})

test_that("sf - error",{
  ee_randomPoints <- ee$FeatureCollection$randomPoints(region, 30000)
  expect_error(ee_as_sf(ee_randomPoints, maxFeatures = 15000))
})



test_that("ee_as_sf - drive - dsn",{
  ee_randomPoints <- ee_as_sf(
    x = ee$Feature(sheds$first()),
    dsn = tempfile(fileext = ".geojson"),
    via = "drive",
    lazy = TRUE
  )
  results <- ee_utils_future_value(ee_randomPoints)
  expect_s3_class(results, "sf")
})


test_that("ee_as_sf - gcs - dsn",{
  ee_randomPoints <- ee_as_sf(
    x = ee$Feature(sheds$first()),
    dsn = tempfile(fileext = ".geojson"),
    container = "rgeedev2",
    via = "gcs",
    lazy = TRUE
  )
  results <- ee_utils_future_value(ee_randomPoints)
  expect_s3_class(results, "sf")
})

test_that("sf - error",{
  ee_randomPoints <- ee$FeatureCollection$randomPoints(region, 30000)
  expect_error(ee_as_sf(ee_randomPoints, via = 15000))
})
