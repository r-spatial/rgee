context("rgee: ee_upload test")

library(rgee)
library(stars)
library(sf)

ee_Initialize(
  email = "data.colec.fbf@gmail.com",
  drive = TRUE,
  gcs = TRUE
)

# ee_upload with bucket -----------------------------------------------------
test_that("ee_upload - character with bucket", {
  filename <- "users/data.colec.fbf/rgee_upload/"
  ee_manage_create(filename)

  tif <- system.file("tif/geomatrix.tif", package = "stars")
  geomatrix <- read_stars(tif) %>% st_warp(crs = st_crs(4326))
  geotiff_file <- paste0(tempfile(), ".tif")
  write_stars(geomatrix, geotiff_file)

  ee_upload(
    x = geotiff_file,
    filename = paste0(filename, "geomatrix"),
    bucket = "rgee_dev"
  )
  ee_geomatrix <- ee$Image(paste0(filename, "geomatrix"))
  geom <- ee$Geometry(ee_geomatrix$geometry()$bounds())
  geomatrix_stars <- ee_as_thumbnail(
    x = ee_geomatrix,
    region = geom,
    vizparams = list(min = 0, max = 255)
  )
  geomatrix_stars[geomatrix_stars <= 0] <- NA
  expect_s3_class(geomatrix_stars, "stars")
})

system.time(2)

test_that("ee_upload - stars with bucket", {
  filename <- "users/data.colec.fbf/rgee_upload/"
  ee_manage_create(filename)

  tif <- system.file("tif/geomatrix.tif", package = "stars")
  geomatrix <- read_stars(tif) %>% st_warp(crs = st_crs(4326))
  delta_geomatrix <- c(attr(geomatrix, "dimensions")$x$delta,
                       attr(geomatrix, "dimensions")$y$delta * -1)

  ee_upload(
    x = geomatrix,
    filename = paste0(filename, "geomatrix"),
    bucket = "rgee_dev"
  )

  ee_geomatrix <- ee$Image(paste0(filename, "geomatrix"))
  geom <- ee$Geometry(ee_geomatrix$geometry()$bounds())
  geomatrix_stars <- ee_as_thumbnail(
    x = ee_geomatrix,
    region = geom,
    vizparams = list(min = 0, max = 255)
  )
  geomatrix_stars[geomatrix_stars <= 0] <- NA
  expect_s3_class(geomatrix_stars, "stars")
})

system.time(2)

test_that("ee_upload - stars-proxy with bucket", {
  filename <- "users/data.colec.fbf/rgee_upload/"
  ee_manage_create(filename)
  tif <- system.file("tif/geomatrix.tif", package = "stars")
  geomatrix <- read_stars(tif) %>% st_warp(crs = st_crs(4326))
  geotiff_file <- paste0(tempfile(), ".tif")
  write_stars(geomatrix, geotiff_file)
  geomatrix_proxy <- read_stars(geotiff_file, proxy = TRUE)

  ee_upload(
    x = geomatrix_proxy,
    filename = paste0(filename, "geomatrix"),
    bucket = "rgee_dev"
  )

  ee_geomatrix <- ee$Image(paste0(filename, "geomatrix"))
  geom <- ee$Geometry(ee_geomatrix$geometry()$bounds())
  geomatrix_stars <- ee_as_thumbnail(
    x = ee_geomatrix,
    region = geom,
    vizparams = list(min = 0, max = 255)
  )
  geomatrix_stars[geomatrix_stars <= 0] <- NA
  expect_s3_class(geomatrix_stars, "stars")
})

# ee_upload without bucket -----------------------------------------------------
# test_that("ee_upload - stars without bucket",{
#   filename <- "users/data.colec.fbf/rgee_upload/"
#   ee_manage_create(filename)
#
#   tif = system.file("tif/geomatrix.tif", package = "stars")
#   geomatrix = read_stars(tif) %>% st_warp(crs=st_crs(4326))
#   expect_error(ee_upload(x = geomatrix,
#                          filename = paste0(filename,"geomatrix")))
#   ee_geomatrix <- ee$Image(paste0(filename,"geomatrix"))
#   geomatrix_stars <- ee_as_thumbnail(x = ee_geomatrix,
#                                      vizparams = list(min = 0, max = 255))
#   geomatrix_stars[geomatrix_stars<=0]=NA
#   expect_s3_class(geomatrix_stars,'stars')
# })
