context("rgee: ee_date test")

# Pre-checking ------------------------------------------------------
# Google credentials were loaded in the system?
skip_if_no_credentials <- function(user) {
  ee_path <- path.expand(sprintf("~/.config/earthengine/%s", user))
  credentials <- list.files(
    path = ee_path,
    pattern = "@gmail.com|credentials|GCS_AUTH_FILE.json"
  )
  if (length(credentials) != 3) {
    skip("All google credentials were not found")
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
    expr = ee$Image()$getInfo(),
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

user <- "data.colec.fbf"
skip_if_no_credentials(user)
skip_if_no_pypkg()
init_rgee()
# -------------------------------------------------------------------------


test_that("rdate_to_eedate I", {
  eedate <- rdate_to_eedate('2000-01-01')
  expect_true('ee.ee_date.Date' %in% class(eedate))
})

test_that("rdate_to_eedate II", {
  eedate <- rdate_to_eedate(date = '2000-01-01', timestamp = TRUE)
  expect_equal(eedate, 946684800000)
})

test_that("eedate_to_rdate I", {
  ee_date <- ee$Date$fromYMD(2017, 2, 3)
  eedate <- eedate_to_rdate(ee_date)
  expect_s3_class(eedate, 'POSIXct')
})

test_that("eedate_to_rdate II", {
  ee_date <- ee$Date$fromYMD(2017, 2, 3)
  eedate <- eedate_to_rdate(ee_date, timestamp = TRUE)
  expect_equal(eedate, 1.48608e+12)
})

test_that("ee_get_date_img", {
  l8 <- ee$Image('LANDSAT/LC08/C01/T1_TOA/LC08_044034_20140318')
  l8_date <- ee_get_date_img(l8)
  expect_equal(l8_date$image_id, "LANDSAT/LC08/C01/T1_TOA/LC08_044034_20140318")
  srtm <- ee$Image('CGIAR/SRTM90_V4')
  srtm_date <- ee_get_date_img(srtm, time_end = TRUE)
  expect_equal(srtm_date$image_id, "CGIAR/SRTM90_V4")
})

test_that("ee_get_date_ic", {
  mod11a2 <- ee$ImageCollection("MODIS/006/MOD11A2")$
    filter(ee$Filter$date('2001-01-01', '2001-03-31'))
  modis_date_01 <- ee_get_date_ic(x = mod11a2)
  modis_date_02 <- ee_get_date_ic(mod11a2,time_end = TRUE)
  expect_s3_class(modis_date_01, "data.frame")
  expect_s3_class(modis_date_02, "data.frame")
})

test_that("ee_get_date_ic", {
  ee_img <- ee$Image(0)
  expect_equal(ee_get_date_img(ee_img)[["image_id"]], "no_id")
  ee_ic <- ee$ImageCollection(c(ee_img, ee_img))
  expect_type(ee_get_date_ic(x = ee_ic)[["id"]], "character")
})
