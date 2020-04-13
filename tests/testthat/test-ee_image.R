context("rgee: sf_as_stars test")

library(rgee)
library(stars)
ee_Initialize(
  email = "data.colec.fbf@gmail.com",
  drive = TRUE,
  gcs = TRUE
)

# Define data -------------------------------------------------------------
img <- ee$Image("LANDSAT/LC08/C01/T1_SR/LC08_038029_20180810")$
  select(c("B4", "B3", "B2"))$
  divide(10000)
geometry <- ee$Geometry$Rectangle(
  coords = c(-110.8, 44.6, -110.6, 44.7),
  proj = "EPSG:4326",
  geodesic = FALSE
)
tif <- system.file("tif/L7_ETMs.tif", package = "stars")
stars_x <- read_stars(tif)
starsproxy_x <- read_stars(tif, proxy = TRUE)
asset_id <- sprintf("%s/%s",ee_get_assethome(),'stars_l7')
image_srtm <- ee$Image("CGIAR/SRTM90_V4")

# Test --------------------------------------------------------------------
test_that("ee_as_proxystars ", {
  proxy_01 <- rgee:::ee_as_proxystars(tif)
  proxy_02 <- rgee:::ee_as_proxystars(stars_x)
  proxy_03 <- rgee:::ee_as_proxystars(starsproxy_x)
  expect_s3_class(proxy_01, 'stars_proxy')
  expect_s3_class(proxy_02, 'stars_proxy')
  expect_s3_class(proxy_03, 'stars_proxy')
})


test_that("ee_as_stars - getInfo ", {
  img_stars_01 <- ee_image_as_stars(
    image = img,
    region = geometry,
    via = "getInfo"
  )
  expect_s3_class(img_stars_01,'stars')
})

test_that("ee_as_stars - drive ", {
  img_stars_02 <- ee_image_as_stars(
    image = img,
    region = geometry,
    via = "drive"
  )
  expect_s3_class(img_stars_02, 'stars')
})

test_that("ee_as_stars - gcs", {
  img_raster_03 <- ee_image_as_raster(
    image = img,
    region = geometry,
    via = "gcs",
    container = 'rgee_dev'
  )
  expect_s4_class(img_raster_03, 'RasterStack')
})


test_that("stars_as_ee - gcs", {
  gs_uri <- ee_local_to_gcs(x = tif, bucket = 'rgee_dev')
  # 2. Pass from gcs to asset
  stars_x <- read_stars(tif)
  st_crs(stars_x) <- 4326
  ee_gcs_to_asset_image(
    x = stars_x,
    gs_uri = gs_uri,
    asset_id = asset_id
  )
  ee_image <- ee$Image(asset_id)
  expect_s3_class(ee_image,'ee.image.Image')
  ee_image_02 <- stars_as_ee(
    x = stars_x,
    assetId = asset_id,
    bucket = "rgee_dev"
  )
  expect_s3_class(ee_image_02,'ee.image.Image')
})

# world image thumbnail ---------------------------------------------------
region <- ee$Geometry$Rectangle(
  coords = c(-180,-60,180,60),
  proj =  "EPSG:4326",
  geodesic = FALSE
)

test_that("ee_as_thumbnail world", {
  world_dem <- ee_as_thumbnail(x = image_srtm, region = region)
  expect_s3_class(world_dem, 'stars')
})

test_that("ee_clean_container", {
  drive <- ee_clean_container()
  gcs <- ee_clean_container(name = 'rgee_dev',type = 'gcs')
  expect_true(gcs)
  expect_true(drive)
})
