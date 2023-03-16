context("rgee: sf_as_ee test")
# -------------------------------------------------------------------------

geom <- ee$Geometry$Point(list(-73.53522, -15.75453))
eeobject_fc <- ee$FeatureCollection(geom)
image <- ee$Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318")
collection <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
  filter(ee$Filter()$eq("WRS_PATH", 44))$
  filter(ee$Filter()$eq("WRS_ROW", 34))$
  filterDate("2014-01-01", "2015-01-01")$
  sort("CLOUD_COVER")

# testing -----------------------------------------------------------------
test_that("Map default", {
  expect_s3_class(Map,'EarthEngineMap')
})


test_that("Map ee_setZoom", {
  zoom <- Map$setZoom()
  expect_type(zoom, "double")
})

test_that("Map ee_setZoom", {
  eeCenter <- Map$setCenter(-10,-20,10)
  # return zoom
  expect_equal(eeCenter, 10)
})

test_that("Map ee_centerObject", {
  eeCenter <- Map$centerObject(
    eeObject = geom,
    zoom = 5,
    maxError = ee$ErrorMargin(1))
  expect_type(eeCenter, "double")
  eeCenter <- Map$centerObject(
    eeObject = image,
    zoom = 7,
    maxError = ee$ErrorMargin(1)
  )
  expect_equal(eeCenter, 7)
})

test_that("Map geometry", {
  m1 <- Map$addLayer(
    geom,
    list(pointRadius = 10, color = "FF0000"),
    "Geometry-Arequipa-test")
  m1_noviz <- Map$addLayer(geom, name =  "Geometry-Arequipa")
  expect_equal(m1$rgee$name, "Geometry-Arequipa-test")
  expect_equal(m1_noviz$rgee$name, "Geometry-Arequipa")
})

test_that("Map geometry", {
  m1 <- Map$addLayer(geom,
                     list(pointRadius = 10, color = "FF0000"),
                     "Geometry-Arequipa-test")
  m1_noviz <- Map$addLayer(geom, name =  "Geometry-Arequipa")
  expect_equal(m1$rgee$name, "Geometry-Arequipa-test")
  expect_equal(m1_noviz$rgee$name, "Geometry-Arequipa")
})

test_that("Map feature", {
  m2 <- Map$addLayer(
    ee$Feature(geom),
    name = "Feature-Arequipa-test"
  )
  expect_equal(m2$rgee$name,"Feature-Arequipa-test")
})

# Case: FeatureCollection
test_that("Map FeatureCollection", {
  m3 <- Map$addLayer(
    eeObject = eeobject_fc,
    name = "FeatureCollection"
  )
  expect_equal(m3$rgee$name,"FeatureCollection")
})

# Case: Image
#test_that("Map Image", {
  # nc <- sf::st_read(system.file("shp/arequipa.shp", package="rgee"))
  # m4 <- rgee:::ee_addLayer(
  #   eeObject = image,
  #   visParams = list(bands = c("B4", "B3", "B2"), max = 10000),
  #   name = "SF"
  # )
  # m6 <- m4 + mapview::mapview(nc)
  # m5 <- mapview::mapview(nc) + m4
  # expect_equal(m4@object$names,"SF")
#})
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
  message <- Map$centerObject(
      eeObject = eeobject_fc)
  expect_equal(message, 18)
  message <- Map$centerObject(
    eeObject = ee$Image(0))
  expect_equal(message, 0)
  message <- Map$centerObject(
    eeObject = eeobject_fc$first()$geometry())
  expect_equal(message, 18)
})

# Test impossible to get center
test_that("Map ee$Image(0)", {
  img <- ee$Image(0)
  m1 <- Map$centerObject(img)
  expect_equal(m1, 0)
})

# Test viz Map
test_that("Map ee$Image", {
  Map$centerObject(image)
  m1 <- Map$addLayer(
    image$normalizedDifference(c("B5", "B4"))
  ) + Map$addLegend(list(min = 0, max = 1))
  expect_s3_class(m1, "leaflet")
})

# ee_get_system_id
test_that("Map ee$Image", {
  # img
  img <- ee$Image(0)$set("system:id", "cesar")
  img_name <- rgee:::ee_get_system_id(img)
  # feat
  ft <- ee$Feature(ee$Geometry$Rectangle(0,0,0,0))$set("system:id", "cesar")
  ft_name <- rgee:::ee_get_system_id(ft)
  # fc
  fc <- ee$FeatureCollection(ft)$set("system:id", "cesar")
  fc_name <- rgee:::ee_get_system_id(fc)
  # ic
  ic <- ee$ImageCollection(list(ee$Image(0), ee$Image(0)))$
    set("system:id", "cesar")
  ic_name <- rgee:::ee_get_system_id(ic)

  expect_equal(ic_name, "cesar")
  expect_equal(img_name, "cesar")
  expect_equal(ft_name, "cesar")
  expect_equal(fc_name, "cesar")
})

# ImageCollection
test_that("Map ee$ImageCollection", {
  ic <- ee$ImageCollection(list(ee$Image(0), ee$Image(0)))
  mx <- Map$addLayers(ic)
  expect_s3_class(mx, "leaflet")
})

# ImageCollection
test_that("Map ee$ImageCollection", {
  e1 <- ee$Image(0)
  e2 <- ee$Image(0)
  mx <- Map$addLayer(e1, name = "Lesly") +
  Map$addLayer(e2, name = "Lesly")
  expect_s3_class(mx, "leaflet")
})

test_that("Map + same name & legend", {
  e1 <- Map$addLayer(ee$Image(0), name = "Lesly")
  e2 <- Map$addLayer(ee$Image(0), name = "Lesly")
  m1 <- e1 + e2 + Map$addLegend(list(min = 0, max = 0, palette = "black"), color_mapping = "discrete")
  expect_s3_class(m1, "leaflet")
})

test_that("Map | comparison operator", {
  e1 <- Map$addLayer(ee$Image(0), name = "Lesly")
  e2 <- Map$addLayer(ee$Image(0), name = "Lesly")
  mc1 <- e1 | e2
  mc2 <- e2 | e1
  expect_s3_class(mc1, "leaflet")
  expect_s3_class(mc2, "leaflet")
})

# ee_mapViewLayersControl
test_that("ee_mapViewLayersControl", {
  map <- Map$addLayer(ee$Image(0))
  map.types <- "Esri.WorldImagery"
  names <- "dss"
  native.crs <- TRUE
  mm5 <- rgee:::ee_mapViewLayersControl(map, map.types, names, native.crs)
  expect_s3_class(mm5, "leaflet")
})

# COG testing
test_that("COG testing", {
  resource <- "https://oin-hotosm.s3.amazonaws.com/56f9b5a963ebf4bc00074e70/0/56f9c2d42b67227a79b4faec.tif"
  visParams <- list(nodata = 0, expression = "B1*1+B2*4+B3*2", rescale = "0, 2000", colormap_name = "viridis")
  Map <- rgee::R6Map$new()
  Map$centerObject(resource)
  m1 <- Map$addLayer(resource, visParams=visParams)
  expect_equal(m1$rgee$opacity, 1)

  resource <- "sss"
  expect_error(Map$centerObject(resource))
  expect_error(Map$addLayer(resource))
})

