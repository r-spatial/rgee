context("rgee: ee_upload test")
skip_if_no_pypkg()
# -------------------------------------------------------------------------

test_that("local_to_gcs - character",{
  skip_if_no_credentials()
  # Define an image.
  tif <- system.file("tif/L7_ETMs.tif", package = "stars")
  gcsuri <- local_to_gcs(x = tif, bucket = gcs_bucket_f())
  gcsuri <- local_to_gcs(x = tif, bucket = gcs_bucket_f(), quiet = TRUE)
  expect_type(gcsuri,'character')
})

# ee_upload with bucket -----------------------------------------------------
test_that("gcs_to_ee_table ", {
  skip_if_no_credentials()
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
  assetId <- sprintf("%s/%s",ee_get_assethome(),'sf_nc')
  zipfile <- ee_utils_shp_to_zip(nc)
  gs_uri <- local_to_gcs(x = zipfile,
                         bucket = gcs_bucket_f())
  manifest <- ee_utils_create_manifest_table(
    gs_uri = gs_uri,
    assetId = assetId
  )
  gcs_to_ee_table(
    manifest = manifest,
    overwrite = TRUE
  )
  ee_monitoring()
  ee_sf_01 <- ee$FeatureCollection(assetId)
  expect_s3_class(object = ee_sf_01,
                  class =  "ee.featurecollection.FeatureCollection")
})


test_that("gcs_to_ee_image ", {
  skip_if_no_credentials()
  # Get the filename of a image
  tif <- system.file("tif/L7_ETMs.tif", package = "stars")
  x <- stars::read_stars(tif)
  st_crs(x) <- 4326
  assetId <- sprintf("%s/%s",ee_get_assethome(),'stars_l7')

  # Method 1
  # 1. Move from local to gcs
  gs_uri <- local_to_gcs(x = tif, bucket = gcs_bucket_f())

  manifest <- ee_utils_create_manifest_image(
    gs_uri = gs_uri,
    assetId = assetId
  )

  # 2. Pass from gcs to asset
  result <- gcs_to_ee_image(
    manifest,
    overwrite = TRUE
  )
  expect_equal(result,sprintf("%s/stars_l7", ee_get_assethome()))
})
