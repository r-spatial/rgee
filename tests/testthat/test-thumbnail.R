context("rgee: ee_as_thumbnail test")

library(raster)
library(rgee)
library(sf)

ee_Initialize(email = 'data.colec.fbf@gmail.com')

### 1. Data
dem_palette <- c(
  "#008435", "#1CAC17", "#48D00C", "#B3E34B", "#F4E467",
  "#F4C84E", "#D59F3C", "#A36D2D", "#C6A889", "#FFFFFF"
)

nc <- st_read(system.file("shp/arequipa.shp", package = "rgee"))
sheds <- ee$FeatureCollection("USGS/WBD/2017/HUC06")$
  filterBounds(ee$Geometry$Rectangle(-127.18, 19.39, -62.75, 51.29))$
  map(function(feature) {
  num <- ee$Number$parse(feature$get("areasqkm"))
  return(feature$set("areasqkm", num))
})
image <- ee$Image("CGIAR/SRTM90_V4")
region <- nc$geometry[[1]] %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_set_crs(4326) %>%
  sf_as_ee() %>%
  ee$FeatureCollection$geometry()

# just one band -----------------------------------------------------------
test_that("ee_as_thumbnail full parameters", {
  arequipa_dem <- ee_as_thumbnail(image = image,
                                  region = region,
                                  raster = TRUE,
                                  vizparams = list(min = 0, max = 5000))
  arequipa_dem <- arequipa_dem * 5000
  expect_equal(max(getValues(arequipa_dem)), 5000)
})

test_that("ee_as_thumbnail min-max", {
  # JPEG images
  mysheds <- ee$Feature(sheds$first())$geometry()
  shed_dem <- ee_as_thumbnail(
    image = image,
    region = mysheds$bounds(),
    vizparams = list(
      min = 0,
      max = 500
    )
  )
  expect_equal(max(shed_dem[[1]]), 0.4470588, tolerance = .002)
})

# RGB band -----------------------------------------------------------
test_that("ee_as_thumbnail palette, min-max", {
  # PNG images
  arequipa_dem <- ee_as_thumbnail(
    image = image,
    region = region,
    vizparams = list(palette = dem_palette, min = 0, max = 5000)
  )
  arequipa_dem <- arequipa_dem * 5000
  expect_equal(max(arequipa_dem[[1]]), 5000, tolerance = 1)
})


# RGB band -----------------------------------------------------------
test_that("ee_as_thumbnail region", {
  # PNG images
  image_clip <- image$clip(region)
  arequipa_dem <- ee_as_thumbnail(
    image = image_clip,
    region = region,
    raster = TRUE,
    vizparams = list(
      palette = dem_palette,
      min = 0,
      max = 5000
    )
  )
  arequipa_dem <- arequipa_dem * 5000
  expect_equal(mean(arequipa_dem[1:10,1:10,3]), 1638.17, tolerance = 1)
})

# error -----------------------------------------------------------
test_that("ee_as_thumbnail error 01", {
  # PNG images
  expect_error(ee_as_thumbnail("ee$Image", region))
})

test_that("ee_as_thumbnail error 02", {
  # PNG images
  expect_error(ee_as_thumbnail(image, "ee$Geometry"))
})


test_that("ee_as_thumbnail error 03", {
  # PNG images
  expect_error(ee_as_thumbnail(image, region$centroid()$buffer(100)))
})


# large image -----------------------------------------------------------
test_that("ee_as_thumbnail large image", {
  # PNG images
  region <- ee$Geometry$Point(-72.403,-16.08)$buffer(100)$bounds()
  arequipa_dem <- ee_as_thumbnail(
    image = image,
    region = region,
    dimensions = 3000L
  )
  expect_s3_class(arequipa_dem, "stars")
})


