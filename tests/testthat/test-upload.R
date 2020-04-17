context("rgee: ee_upload test")

ee_Initialize(
  email = "data.colec.fbf@gmail.com",
  drive = TRUE,
  gcs = TRUE
)

test_that("ee_local_to_gcs - character",{
  # Define an image.
  tif <- system.file("tif/L7_ETMs.tif", package = "stars")
  gcsuri <- ee_local_to_gcs(x = tif, bucket = 'rgee_dev')
  gcsuri <- ee_local_to_gcs(x = tif, bucket = 'rgee_dev',quiet = TRUE)
  expect_type(gcsuri,'character')
})

# ee_upload with bucket -----------------------------------------------------
test_that("ee_gcs_to_table ", {
  nc <- st_read(system.file("shape/nc.shp", package = "sf"))
  asset_id <- sprintf("%s/%s",ee_get_assethome(),'sf_nc')
  zipfile <- ee_create_shp_zip(nc)
  gs_uri <- ee_local_to_gcs(x = zipfile,
                            bucket = 'rgee_dev')
  ee_gcs_to_table(
    gs_uri = gs_uri,
    asset_id = asset_id
  )
  #ee_monitoring()
  ee_sf_01 <- ee$FeatureCollection(asset_id)
  expect_s3_class(object = ee_sf_01,
                  class =  "ee.featurecollection.FeatureCollection")
})

system.time(2)

test_that("ee_gcs_to_image ", {
  # Get the filename of a image
  tif <- system.file("tif/L7_ETMs.tif", package = "stars")
  x <- read_stars(tif)
  st_crs(x) <- 4326
  asset_id <- sprintf("%s/%s",ee_get_assethome(),'stars_l7')

  # Method 1
  # 1. Move from local to gcs
  gs_uri <- ee_local_to_gcs(x = tif, bucket = 'rgee_dev')

  # 2. Pass from gcs to asset
  result <- ee_gcs_to_image(
    x = x,
    gs_uri = gs_uri,
    asset_id = asset_id
  )
  expect_equal(result,0)
})
