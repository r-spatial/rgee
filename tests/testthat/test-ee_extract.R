context("rgee: ee_extract test")

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
