context("rgee: sf_as_stars test")

library(rgee)
library(raster)
library(stars)
ee_reattach()
ee_Initialize(email = 'data.colec.fbf@gmail.com', drive = TRUE, gcs = TRUE)

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
assetId <- sprintf("%s/%s",ee_get_assethome(),'stars_l7')
image_srtm <- ee$Image("CGIAR/SRTM90_V4")

# getInfo geometry ---------------------------------------------------
test_that('Geometry consideration for "getInfo"', {
  rect_01 <- ee$Geometry$Rectangle(-3, -3, 3, 3)
  test_image_01 <- ee$Image(0)$add(1)

  # test01: base case
  img_01 <- ee_as_raster(image = test_image_01, region = rect_01,
                               quiet = TRUE)
  expect_equal(extent(img_01), extent(-3,3,-3,3))

  # test02: even a small increase will add a new pixel
  nominalscale = test_image_01$projection()$nominalScale()$getInfo()
  test_image_02 <- test_image_01$reproject(
    crs = "EPSG:4326",
    scale = round(nominalscale)
  )
  img_02 <- ee_as_raster(image = test_image_02, region = rect_01,
                               quiet = TRUE)
  expect_equal(dim(img_02), c(8, 8, 1))
})

# ee_Image_local ---------------------------------------------------
test_that("ee_as_proxystars ", {
  proxy_01 <- rgee:::ee_as_proxystars(tif)
  proxy_02 <- rgee:::ee_as_proxystars(stars_x)
  proxy_03 <- rgee:::ee_as_proxystars(starsproxy_x)
  expect_s3_class(proxy_01, 'stars_proxy')
  expect_s3_class(proxy_02, 'stars_proxy')
  expect_s3_class(proxy_03, 'stars_proxy')
})

test_that("ee_as_stars - simple ", {
  #getinfo
  img_stars_01 <- ee_as_stars(
    image = img,
    region = geometry,
    via = "getInfo"
  )
  expect_s3_class(img_stars_01,'stars')

  #drive
  img_stars_02 <- ee_as_stars(
    image = img,
    region = geometry,
    via = "drive"
  )
  expect_s3_class(img_stars_02, 'stars')

  img_raster_03 <- ee_as_raster(
    image = img,
    region = geometry,
    via = "gcs",
    container = 'rgee_dev'
  )
  expect_s4_class(img_raster_03, 'RasterStack')

  getInfo <- mean(getValues(raster(img_stars_01[[1]])))
  drive <- mean(getValues(raster(img_stars_02[[1]])),na.rm = TRUE)
  # Equal value but some problems in the bounds
  expect_equal(getInfo, drive, tolerance = 0.1)
})



# test_that("ee to drive to local - gcs", {
#   try(ee_manage_delete(assetId), silent = TRUE)
#   gs_uri <- ee_local_to_gcs(x = tif, bucket = 'rgee_dev')
#   # 2. Pass from gcs to asset
#   stars_x <- read_stars(tif)
#   gcs_to_ee_image(
#     x = stars_x,
#     gs_uri = gs_uri,
#     assetId = assetId
#   )
#   ee_monitoring()
#   ee_image <- ee$Image(assetId)
#   expect_s3_class(ee_image,'ee.image.Image')
#   try(ee_manage_delete(assetId), silent = TRUE)
#   ee_image_02 <- stars_as_ee(
#     x = stars_x,
#     assetId = assetId,
#     bucket = "rgee_dev"
#   )
#   expect_s3_class(ee_image_02,'ee.image.Image')
#   try(ee_manage_delete(assetId), silent = TRUE)
#   ee_image_03 <- stars_as_ee(
#     x = stars_x,
#     assetId = assetId,
#     bucket = "rgee_dev",
#     monitoring = FALSE
#   )
#   expect_type(ee_image_03,'character')
# })

test_that("ee_as_raster", {
  img_01 <- ee_as_raster(
    image = image_srtm,
    region = geometry,
    scale = 10,
    via = "getInfo"
  )
  expect_s4_class(img_01, "RasterStack")
})

# world image thumbnail -----------------------------------------------
region <- ee$Geometry$Rectangle(
  coords = c(-180,-60,180,60),
  proj =  "EPSG:4326",
  geodesic = FALSE
)

test_that("ee_as_thumbnail world", {
  world_dem <- ee_as_thumbnail(image = image_srtm, region = region)
  expect_s3_class(world_dem, 'stars')
})

# clean containers  ---------------------------------------------------
test_that("ee_clean_container", {
  drive <- ee_clean_container()
  gcs <- ee_clean_container(name = 'rgee_dev',type = 'gcs')
  expect_true(gcs)
  expect_true(drive)
})

# errors  ------------------------------------------------------------
test_that("ee_image_local error 1", {
  expect_error(
    rgee:::ee_image_local(
      image = "ee$Image",
      region = geometry,
      scale = 100,
      via = "getInfo"
      )
    )
  }
)


test_that("ee_image_local error 2", {
  expect_error(
    rgee:::ee_image_local(
      image = image_srtm,
      region = image_srtm,
      scale = 100,
      via = "getInfo"
      )
    )
  }
)

test_that("ee_image_local error 3", {
  expect_error(
    rgee:::ee_image_local(
      image = image_srtm,
      region = geometry$centroid(maxError = 1)$buffer(100),
      scale = 100,
      via = "getInfo"
    )
  )
}
)

test_that("ee_image_local error 4", {
  expect_error(
    rgee:::ee_image_local(
      image = image_srtm,
      region = geometry,
      scale = "100",
      via = "getInfo"
    )
  )
}
)


test_that("ee_image_local error 5", {
  expect_error(
    rgee:::ee_image_local(
      image = image_srtm,
      region = geometry,
      scale = c(100, 120),
      via = "getInfo"
    )
  )
}
)

test_that("ee_image_local error 6", {
  expect_error(
    rgee:::ee_image_local(
      image = image_srtm,
      region = geometry,
      via = "testing"
    )
  )
}
)


test_that("ee_image_local error 7", {
  expect_error(
    rgee:::ee_image_local(
      image = image_srtm,
      region = geometry,
      scale = 0.1,
      via = "getInfo"
    )
  )
}
)

test_that("ee_image_local error 8", {
  expect_error(
  ee_as_raster(
    image = image_srtm,
    region = geometry,
    scale = 20000,
    via = "getInfo"
  )
  )
})

test_that("ee_image_local error 9", {
  GEOtransform <- image_srtm$projection()$getInfo()$transform
  GEOtransform[[2]] <- 10
  GEOtransform[[4]] <- 10
  expect_error(
    ee_as_raster(
      image = image_srtm$reproject(crs = "EPSG:4326",
                                   crsTransform = GEOtransform),
      region = geometry,
      scale = 20000,
      via = "getInfo"
    )
  )
 }
)


# ee_image_info test
test_that("ee_image_info", {
  # World SRTM
  srtm <- ee$Image("CGIAR/SRTM90_V4")
  srtm_list <- ee_image_info(srtm)
  # Landast8
  l8 <- ee$Image("LANDSAT/LC08/C01/T1_SR/LC08_038029_20180810")
  l8_list <- ee_image_info(l8,getsize = FALSE)
  expect_type(srtm_list, "list")
  expect_type(l8_list, "list")
})
