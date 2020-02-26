context("rgee: sf_as_ee test")

# data --------------------------------------------------------------------
ee$Initialize()
geom <- ee$Geometry$Point(list(-73.53522, -15.75453))
eeobject_fc <- ee$FeatureCollection("users/csaybar/DLdemos/train_set")
image <- ee$Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318")
collection <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
  filter(ee$Filter()$eq("WRS_PATH", 44))$
  filter(ee$Filter()$eq("WRS_ROW", 34))$
  filterDate("2014-01-01", "2015-01-01")$
  sort("CLOUD_COVER")

# testing -----------------------------------------------------------------
test_that("ee_map default", {
  expect_s4_class(rgee:::ee_mapview(),'mapview')
})

test_that("ee_map geometry", {
  m1 <- ee_Map$addLayer(geom,
                        list(pointRadius = 10, color = "FF0000"),
                        "Geometry-Arequipa-test")
  m1_noviz <- ee_Map$addLayer(geom,name =  "Geometry-Arequipa")
  expect_equal(m1@object$names, "Geometry-Arequipa-test")
  expect_equal(m1_noviz@object$names, "Geometry-Arequipa")
})

test_that("ee_map feature", {
  m2 <- ee_Map$addLayer(ee$Feature(geom),
                        name = "Feature-Arequipa-test")
  expect_equal(m2@object$names,"Feature-Arequipa-test")
})

# Case: FeatureCollection
test_that("ee_map FeatureCollection", {
  m3 <- ee_Map$addLayer(eeObject = eeobject_fc,
                        name = "FeatureCollection")
  expect_equal(m3@object$names,"FeatureCollection")
})

# Case: Image
test_that("ee_map Image", {
  m4 <- ee_Map$addLayer(eeObject = image,
                        visParams = list(bands = c("B4", "B3", "B2"),
                                         max = 10000),
                        name = "SF")
  expect_equal(m4@object$names,"SF")
})


test_that("ee_Map$centerObject", {
  ee_Map$centerObject(eeObject = image)
  expect_equal(ee_Map$lat, 37.4716,tolerance = .001)
})

test_that("ee_Map$centerObject", {
  ee_Map$setZoom(zoom = 10)
  expect_equal(ee_Map$zoom, 10)
})

test_that("ee_Map$centerObject", {
  ee_Map$setCenter(lon = 10,lat = 10)
  expect_equal(ee_Map$lon, 10)
  expect_equal(ee_Map$lat, 10)
})
