context("rgee: ee_imagecollection_to_local test")
skip_if_no_pypkg()
# -------------------------------------------------------------------------

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
    ic = collection %>% ee_get(0:1),
    region = geometry,
    scale = 500,
    dsn = tmp,
    quiet = TRUE
  )
  raster(ic_getinfo_files[[1]]$dsn)
  expect_type(ic_getinfo_files, "list")
  }
)

test_that("ee_imagecollection_to_local - simple dsn", {
  ic_getinfo_files <- ee_imagecollection_to_local(
    ic = collection %>% ee_get(0:1),
    region = geometry,
    scale = 500,
    dsn = file.path(tmp, "drive_"),
    quiet = FALSE
  )
  expect_type(ic_getinfo_files, "list")
 }
)

test_that("ee_imagecollection_to_local - simple dsn", {
 ic <- ee$ImageCollection(c(ee$Image(0), ee$Image(1)))
 ic_getinfo_files <- ee_imagecollection_to_local(
   ic = ic,
   scale = 200,
   dsn = c("lesly_01.tif", "lesly_02.tif"),
   region = geometry,
   add_metadata = FALSE
 )
 ic_getinfo_files <- ee_imagecollection_to_local(
   ic = ic,
   scale = 200,
   region = geometry
 )
 expect_type(ic_getinfo_files, "list")
 }
)

test_that("ee_imagecollection_to_local - lazy - simple dsn", {
  # USDA example
  loc <- ee$Geometry$Point(-99.2222, 46.7816)
  collection <- ee$ImageCollection('USDA/NAIP/DOQQ')$
    filterBounds(loc)$
    filterDate('2018-01-01', '2020-01-01')$
    filter(ee$Filter$listContains("system:band_names", "N"))

  # From ImageCollection to local directory
  ee_crs <- collection$first()$projection()$getInfo()$crs
  geometry <- collection$first()$geometry(proj = ee_crs)$bounds()
  tmp <- tempdir()

  ## Using drive
  # one by once
  ic_drive_files_1 <- ee_imagecollection_to_local(
    ic = collection,
    region = geometry,
    scale = 250,
    lazy = TRUE,
    via = "drive",
    dsn = file.path(tmp, "drive_")
  )
  results_list <- ic_drive_files_1 %>% ee_utils_future_value()
  expect_is(ic_drive_files_1, "list")
})
