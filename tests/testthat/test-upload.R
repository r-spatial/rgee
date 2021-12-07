context("rgee: ee_upload test")
skip_if_no_pypkg()
# -------------------------------------------------------------------------

ee_Initialize(gcs = TRUE, drive = TRUE)

test_that("local_to_gcs - character - fine grained access",{
  skip_if_no_credentials()
  # Define an image.
  tif <- system.file("tif/L7_ETMs.tif", package = "stars")
  gcsuri <- local_to_gcs(x = tif, bucket = gcs_bucket_f(), predefinedAcl = "private")
  gcsuri <- local_to_gcs(x = tif, bucket = gcs_bucket_f(), predefinedAcl = "private", quiet = TRUE)
  expect_type(gcsuri,'character')
})


test_that("local_to_gcs - character - uniform access",{
  skip_if_no_credentials()
  # Define an image.
  tif <- system.file("tif/L7_ETMs.tif", package = "stars")
  gcsuri <- local_to_gcs(x = tif, bucket = gcs_bucket_uniform_f(), predefinedAcl = "bucketLevel")
  gcsuri <- local_to_gcs(x = tif, bucket = gcs_bucket_uniform_f(), predefinedAcl = "bucketLevel", quiet = TRUE)
  expect_type(gcsuri,'character')
})

test_that("local_to_gcs errors for mismatched predefinedAcl",{
  skip_if_no_credentials()
  # Define an image.
  tif <- system.file("tif/L7_ETMs.tif", package = "stars")
  expect_error(gcsuri <- local_to_gcs(x = tif, bucket = gcs_bucket_uniform_f(), predefinedAcl = "private"))
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


test_that("ee_as_proxystars ", {
  tif <- system.file("tif/L7_ETMs.tif", package = "stars")
  x <- suppressWarnings(raster::raster(tif))
  xx <- suppressWarnings(rgee:::ee_as_proxystars(x))
  expect_s3_class(xx, "stars")
  expect_error(rgee:::ee_as_proxystars(list(a=10)))
})
