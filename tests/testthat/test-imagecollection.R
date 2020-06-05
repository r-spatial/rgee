## Not run:
library(rgee)
library(raster)

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
  tryCatch(
    expr = ee$Image()$getInfo(),
    error = function(e) {
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


# Dataset
# ---------------------------
loc <- ee$Geometry$Point(-99.2222, 46.7816)
collection <- ee$ImageCollection('USDA/NAIP/DOQQ')$
  filterBounds(loc)$
  filterDate('2008-01-01', '2020-01-01')$
  filter(ee$Filter$listContains("system:band_names", "N"))

# From ImageCollection to local directory
ee_crs <- collection$first()$projection()$getInfo()$crs
geometry <- collection$first()$geometry(proj = ee_crs)$bounds()
tmp <- tempdir()

test_that("ee_imagecollection_to_local - simple dsn", {
  ic_getinfo_files <- ee_imagecollection_to_local(
    ic = collection,
    region = geometry,
    scale = 250,
    dsn = tmp,
    via = "getInfo",
    quiet = TRUE
    )
  expect_type(ic_getinfo_files, "character")
  }
)

test_that("ee_imagecollection_to_local - simple dsn", {
  ic_getinfo_files <- ee_imagecollection_to_local(
    ic = collection,
    region = geometry,
    scale = 250,
    dsn = file.path(tmp, "drive_"),
    via = "getInfo",
    quiet = FALSE
  )
  expect_type(ic_getinfo_files, "character")
 }
)



test_that("ee_imagecollection_to_local - simple dsn", {
 ic <- ee$ImageCollection(c(ee$Image(0), ee$Image(1)))
 ic_getinfo_files <- ee_imagecollection_to_local(
   ic = ic,
   scale = 100,
   dsn = c("lesly_01.tif", "lesly_02.tif"),
   region = geometry
 )
 ic_getinfo_files <- ee_imagecollection_to_local(
   ic = ic,
   scale = 100,
   region = geometry
 )
 expect_type(ic_getinfo_files, "character")
 }
)
