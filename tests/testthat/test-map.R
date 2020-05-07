context("rgee: sf_as_ee test")

ee_path <- path.expand("~/.config/earthengine")
sessioninfo <- sprintf("%s/rgee_sessioninfo.txt", ee_path)

user <- tryCatch(
  expr = read.table(sessioninfo,header = TRUE,stringsAsFactors = FALSE),
  error = function(e) ee_Initialize(
    email = 'data.colec.fbf@gmail.com',
    drive = TRUE,
    gcs = TRUE
  )
)

if (anyNA(user)) {
  ee_reattach()
  ee_Initialize(
    email = 'data.colec.fbf@gmail.com',
    drive = TRUE,
    gcs = TRUE
  )
}

# data --------------------------------------------------------------------
geom <- ee$Geometry$Point(list(-73.53522, -15.75453))
eeobject_fc <- ee$FeatureCollection("users/csaybar/DLdemos/train_set")
image <- ee$Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318")
collection <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
  filter(ee$Filter()$eq("WRS_PATH", 44))$
  filter(ee$Filter()$eq("WRS_ROW", 34))$
  filterDate("2014-01-01", "2015-01-01")$
  sort("CLOUD_COVER")

# testing -----------------------------------------------------------------
test_that("Map default", {
  expect_s4_class(rgee:::ee_mapview(),'mapview')
})

test_that("Map default", {
  Map <- new.env(parent = emptyenv())
  Map <- rgee:::ee_set_methods()
  expect_type(Map, "environment")
})

test_that("Map ee_setZoom", {
  zoom <- rgee:::ee_setZoom(10)
  expect_type(zoom, "double")
})

test_that("Map ee_setZoom", {
  eeCenter <- rgee:::ee_setCenter(-10,-20,10)
  expect_type(eeCenter, "environment")
})


test_that("Map ee_centerObject", {
  eeCenter <- rgee:::ee_centerObject(
    eeObject = geom,
    zoom = 5,
    maxError = ee$ErrorMargin(1))
  expect_type(eeCenter$zoom, "double")
  eeCenter <- rgee:::ee_centerObject(
    eeObject = image,
    zoom = 7,
    maxError = ee$ErrorMargin(1))
  expect_equal(eeCenter$zoom, 7)
})

test_that("Map geometry", {
  m1 <- rgee:::ee_addLayer(
    geom,
    list(pointRadius = 10, color = "FF0000"),
    "Geometry-Arequipa-test")
  m1_noviz <- rgee:::ee_addLayer(geom,name =  "Geometry-Arequipa")
  expect_equal(m1@object$names, "Geometry-Arequipa-test")
  expect_equal(m1_noviz@object$names, "Geometry-Arequipa")
})

test_that("Map geometry", {
  m1 <- rgee:::ee_addLayer(geom,
                           list(pointRadius = 10, color = "FF0000"),
                           "Geometry-Arequipa-test")
  m1_noviz <- rgee:::ee_addLayer(geom,name =  "Geometry-Arequipa")
  expect_equal(m1@object$names, "Geometry-Arequipa-test")
  expect_equal(m1_noviz@object$names, "Geometry-Arequipa")
})

test_that("Map feature", {
  m2 <- rgee:::ee_addLayer(
    ee$Feature(geom),
    name = "Feature-Arequipa-test"
  )
  expect_equal(m2@object$names,"Feature-Arequipa-test")
})

# Case: FeatureCollection
test_that("Map FeatureCollection", {
  m3 <- rgee:::ee_addLayer(
    eeObject = eeobject_fc,
    name = "FeatureCollection"
  )
  expect_equal(m3@object$names,"FeatureCollection")
})

# Case: Image
test_that("Map Image", {
  m4 <- rgee:::ee_addLayer(
    eeObject = image,
    visParams = list(bands = c("B4", "B3", "B2"), max = 10000),
    name = "SF"
  )
  m4 + m4
  expect_equal(m4@object$names,"SF")
})


test_that("Map$centerObject", {
  rgee:::ee_centerObject(eeObject = image)
  expect_equal(Map$lat, 37.4716,tolerance = .001)
})

test_that("Map$centerObject", {
  rgee:::ee_setZoom(zoom = 10)
  expect_equal(Map$zoom, 10)
})

test_that("Map$centerObject", {
  rgee:::ee_setCenter(lon = 10,lat = 10)
  expect_equal(Map$lon, 10)
  expect_equal(Map$lat, 10)
})

test_that("Map$centerObject", {
  sp_01 <- rgee:::ee_get_spatial_objects("Image")
  sp_02 <- rgee:::ee_get_spatial_objects("ImageCollection")
  sp_03 <- rgee:::ee_get_spatial_objects("justfeature")
  sp_04 <- rgee:::ee_get_spatial_objects("All")
  expect_type(sp_01, "character")
  expect_type(sp_02, "character")
  expect_type(sp_03, "character")
  expect_type(sp_04, "character")
})

# Case: FeatureCollection
test_that("ERROR 01", {
  expect_error(
    rgee:::ee_addLayer(
      eeObject = eeobject_fc$toList(1)
    )
  )
  expect_error(
    rgee:::ee_centerObject(
      eeObject = ee$List(c(1,2,3))
      )
  )
})

# Case: FeatureCollection
test_that("messages 01", {
  message <- rgee:::ee_centerObject(
      eeObject = eeobject_fc)
  expect_type(message,"environment")
  message <- rgee:::ee_centerObject(
    eeObject = ee$Image(0))
  expect_type(message,"environment")
  message <- rgee:::ee_centerObject(
    eeObject = eeobject_fc$first()$geometry())
  expect_type(message,"environment")
})


