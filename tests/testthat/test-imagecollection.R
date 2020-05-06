## Not run:
library(rgee)
library(raster)

# Initialize a specific Earth Engine account and load
# either Google Drive or Google Cloud Storage credentials
if (isFALSE(exists('ee'))) {
  ee_reattach()
  ee_Initialize(
    email = 'data.colec.fbf@gmail.com',
    drive = TRUE,
    gcs = TRUE
  )
}

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
