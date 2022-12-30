context("rgee: stars_as_ee test")
# -------------------------------------------------------------------------
ee_Initialize(gcs = TRUE, drive = TRUE)
library(stars)

tif <- system.file("tif/L7_ETMs.tif", package = "stars")
x <- read_stars(tif)
assetId <- sprintf("%s/%s",ee_get_assethome(),'stars_l7')


test_that("stars_as_ee", {
  ee_stars_01 <- stars_as_ee(
    x = x,
    overwrite = TRUE,
    assetId = assetId,
    bucket = "rgeedev2"
  )

  ee_stars_02 <- stars_as_ee(
    x = x,
    overwrite = TRUE,
    assetId = assetId,
    bucket = "rgeedev2",
    monitoring = FALSE
  )
  expect_s3_class(ee_stars_01, "ee.image.Image")
  expect_s3_class(ee_stars_02, "ee.image.Image")
})
