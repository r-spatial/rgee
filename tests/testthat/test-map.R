context("rgee: sf_as_ee test")

# data --------------------------------------------------------------------
ee_Initialize()
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

test_that("Map geometry", {
  m1 <- Map$addLayer(geom,
                        list(pointRadius = 10, color = "FF0000"),
                        "Geometry-Arequipa-test")
  m1_noviz <- Map$addLayer(geom,name =  "Geometry-Arequipa")
  expect_equal(m1@object$names, "Geometry-Arequipa-test")
  expect_equal(m1_noviz@object$names, "Geometry-Arequipa")
})

test_that("Map feature", {
  m2 <- Map$addLayer(ee$Feature(geom),
                        name = "Feature-Arequipa-test")
  expect_equal(m2@object$names,"Feature-Arequipa-test")
})

# Case: FeatureCollection
test_that("Map FeatureCollection", {
  m3 <- Map$addLayer(eeObject = eeobject_fc,
                        name = "FeatureCollection")
  expect_equal(m3@object$names,"FeatureCollection")
})

# Case: Image
test_that("Map Image", {
  m4 <- Map$addLayer(eeObject = image,
                        visParams = list(bands = c("B4", "B3", "B2"),
                                         max = 10000),
                        name = "SF")
  expect_equal(m4@object$names,"SF")
})


test_that("Map$centerObject", {
  Map$centerObject(eeObject = image)
  expect_equal(Map$lat, 37.4716,tolerance = .001)
})

test_that("Map$centerObject", {
  Map$setZoom(zoom = 10)
  expect_equal(Map$zoom, 10)
})

test_that("Map$centerObject", {
  Map$setCenter(lon = 10,lat = 10)
  expect_equal(Map$lon, 10)
  expect_equal(Map$lat, 10)
})
