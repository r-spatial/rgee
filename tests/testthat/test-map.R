context("rgee: sf_as_ee test")

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
test_that("ee_map default", {
  expect_s4_class(ee_map(),'mapview')
})

test_that("ee_map geometry", {
  m1 <- ee_map(
    eeobject = geom,
    vizparams = list(pointRadius = 10, color = "FF0000"),
    objname = "Geometry-Arequipa")
  m1_noviz <- ee_map(
    eeobject = geom,
    objname = "Geometry-Arequipa")
  expect_equal(m1@object$eeobject,"Geometry")
  expect_equal(m1_noviz@object$eeobject,"Geometry")
})

test_that("ee_map feature", {
  m2 <- ee_map(eeobject = ee$Feature(eeobject_fc$first()),
               objname = "Feature-Arequipa")
  expect_equal(m2@object$eeobject,"Feature")
})

# Case: FeatureCollection
test_that("ee_map FeatureCollection", {
  m3 <- ee_map(eeobject = eeobject_fc,
               objname = "FeatureCollection")
  expect_equal(m3@object$eeobject,"FeatureCollection")
})

# Case: Image
test_that("ee_map Image", {
  m4 <- ee_map(
    eeobject = image,
    vizparams = list(
      bands = c("B4", "B3", "B2"),
      max = 10000),
    objname = "SF",
    zoom_start = "8")
  m4noviz <- ee_map(
    eeobject = image,
    zoom_start = "8")
  expect_equal(m4@object$eeobject,"Image")
  expect_equal(m4noviz@object$eeobject,"Image")
})

# Case: ImageCollection
test_that("ee_map ImageCollection", {
  m5 <- ee_map(
    eeobject = collection,
    vizparams = list(bands = c("B4", "B3", "B2"), max = 1),
    objname = c("Scene_2019", "Scene_2016", "Scene_2011"),
    max_nimage = 3,
    zoom_start = 10)
  m5noviz <- ee_map(
    eeobject = collection,
    objname = c("Scene_2019", "Scene_2016", "Scene_2011"),
    max_nimage = 3,
    zoom_start = 10)
  expect_equal(m5@object$eeobject,"ImageCollection")
  expect_equal(m5noviz@object$eeobject,"ImageCollection")
  }
)
