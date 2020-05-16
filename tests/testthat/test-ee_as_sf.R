context("rgee: ee_as_sf test")

# Pre-checking ------------------------------------------------------
# Google credentials were loaded in the system?
skip_if_no_credentials <- function() {
  ee_path <- path.expand("~/.config/earthengine")
  sessioninfo <- sprintf("%s/rgee_sessioninfo.txt", ee_path)
  if (isFALSE(file.exists(sessioninfo))) {
    skip("google credentials were not found")
  }
}

# Neccesary Python packages were loaded?
skip_if_no_pypkg <- function() {
  have_ee <- reticulate::py_module_available("ee")
  have_numpy <- reticulate::py_module_available("numpy")
  if (isFALSE(have_ee)) {
    skip("ee not available for testing")
  }
  if (isFALSE(have_numpy)) {
    skip("numpy not available for testing")
  }
}

# Init Earth Engine just if it is necessary
init_rgee <- function() {
  ee_reattach()
  tryCatch(
    expr = ee$data$get_persistent_credentials()$client_id,
    error = function(e) {
      ee_reattach()
      ee_Initialize(
        email = 'data.colec.fbf@gmail.com',
        drive = TRUE,
        gcs = TRUE
      )
    }
  )
}

skip_if_no_credentials()
skip_if_no_pypkg()
init_rgee()

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
                      container = "rgee_dev")
  expect_s3_class(mysheds,"sf")
})

test_that("sf - error",{
  ee_randomPoints <- ee$FeatureCollection$randomPoints(region, 30000)
  expect_error(ee_as_sf(ee_randomPoints, maxFeatures = 15000))
})

