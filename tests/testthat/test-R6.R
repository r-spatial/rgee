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

## new
test_that("Map new", {
  Map <- rgee::R6Map$new()
  expect_s3_class(Map,'EarthEngineMap')
})


## reset
test_that("Map reset", {
  Map <- rgee::R6Map$new()
  Map$addLayer(ee$Image$Dataset$ESA_GLOBCOVER_L4_200901_200912_V2_3)
  Map$reset()
  expect_s3_class(Map,'EarthEngineMap')
})


## print
test_that("Map print", {
  Map <- rgee::R6Map$new()
  Map$addLayer(ee$Image$Dataset$ESA_GLOBCOVER_L4_200901_200912_V2_3)
  Map$print()
  expect_s3_class(Map,'EarthEngineMap')
})

## setCenter
test_that("Map setCenter", {
  Map <- rgee::R6Map$new()
  Map$addLayer(ee$Image$Dataset$ESA_GLOBCOVER_L4_200901_200912_V2_3)
  Map$setCenter()
  expect_s3_class(Map,'EarthEngineMap')
})

## setCenter
test_that("Map setCenter", {
  Map <- rgee::R6Map$new()
  Map$addLayer(ee$Image$Dataset$ESA_GLOBCOVER_L4_200901_200912_V2_3)
  Map$setCenter()
  expect_s3_class(Map,'EarthEngineMap')
})


## setZoom
test_that("Map setZoom", {
  Map <- rgee::R6Map$new()
  Map$addLayer(ee$Image$Dataset$ESA_GLOBCOVER_L4_200901_200912_V2_3)
  Map$setZoom()
  expect_s3_class(Map,'EarthEngineMap')
})



## centerObject
test_that("Map setZoom", {
  Map <- rgee::R6Map$new()
  imgx <- ee$Image$Dataset$ESA_GLOBCOVER_L4_200901_200912_V2_3
  Map$centerObject(imgx)
  Map$addLayer(imgx)
  expect_s3_class(Map,'EarthEngineMap')
})


## MapaddLayer
test_that("Map setZoom", {
  Map <- rgee::R6Map$new()
  imgx <- ee$Image$Dataset$ESA_GLOBCOVER_L4_200901_200912_V2_3
  Map$addLayer(imgx)
  Map
  expect_s3_class(Map,'EarthEngineMap')
})


## ee_get_spatial_objects
test_that("ee_get_spatial_objects", {
  fls <- rgee:::ee_get_spatial_objects("i+ic")
  expect_equal(fls, c("ee.image.Image", "ee.imagecollection.ImageCollection"))
})



## ee_get_system_id
test_that("ee_get_system_id", {
  ee_list <- ee$List(1:10)
  expect_error(rgee:::ee_get_system_id(ee_list))
})


## R6 + Operators
test_that("R6 + Operators", {
  Map <- rgee::R6Map$new()
  img01 <- Map$addLayer(ee$Image(0))
  img02 <- Map$addLayer(ee$Image(1))
  img03 <- Map$addLayer(ee$Image(2))
  img04 <- (img01 | img02) + img03
  img05 <- (img01 | img02) + img02
  expect_s3_class(img04 + img05, "EarthEngineMap")
})



## errors
test_that("right map +", {
  Map <- rgee::R6Map$new()
  img01 <- Map$addLayer(ee$Image(0))
  expect_error(img01 + 1, "EarthEngineMap")
})

test_that("right map |", {
  Map <- rgee::R6Map$new()
  img01 <- Map$addLayer(ee$Image(0))
  expect_error(img01 | 1, "EarthEngineMap")
})


## Map$addLayer + Map$addLegend
test_that("addLayer | addLegend", {
  Map <- rgee::R6Map$new()
  ee_img <- ee$Image(1)
  m1 <- Map$addLayer(ee_img)
  l1 <- Map$addLegend(list(min=0,max=1), position = "topright")
  l2 <- Map$addLegend(list(min=0,max=1), position = "topright", color_mapping = "discrete")
  m2 <- m1 + l1
  m3 <- m1 + l2
  m4 <- (m2 | m3) + m2
  l3 <- Map$addLegend(list(min=0,max=1), position = "topleft", color_mapping = "discrete")
  m5 <- (m2 | m3) + l3
  expect_type(m4$rgee$legend_params, "list")
  expect_type(m5$rgee$legend_params, "list")
})


## Map$addLayers
test_that("addLayers", {
  Map <- rgee::R6Map$new()
  ic <- ee$ImageCollection(lapply(1:3, ee$Image))
  Map$addLayers(ic, name = c("d", "dd", "d1", "dd"))
  expect_error(Map$addLayers(1))
  Map <- rgee::R6Map$new()
  Map$addLayers(ic)
  expect_s3_class(Map, "EarthEngineMap")
})


## Map$addLegend
test_that("addLegend", {
  Map <- rgee::R6Map$new()
  Map$addLayer(ee$Image(0))
  Map$addLegend(list(min = 0, max = 1),"c", "bottomleft", "discrete")
  Map$addLegend(list(min = 0, max = 1),"c", "topleft", "numeric")
  vizparam <- list(min = 0, max = 1 , values = c("negro", "blanco"))
  Map$addLegend(vizparam,"cs", color_mapping = "character")
  expect_error(Map$addLegend(vizparam,"cs", color_mapping = "dd"))
  expect_error(Map$addLegend(list(min = 0, max = 1),"cs", color_mapping = "character"))
  expect_s3_class(Map, "EarthEngineMap")
})



## R6Map print
test_that("Map$print I", {
  Map <- rgee::R6Map$new()
  Map$addLayer(ee$Image(0), position = "left")
  Map$addLayer(ee$Image(0), position = "right")
  expect_s3_class(Map, "EarthEngineMap")

  Map <- rgee::R6Map$new(save_maps = FALSE)
  Map$addLayer(ee$Image(0))
  expect_s3_class(Map, "EarthEngineMap")
})

## R6Map set errors
test_that("Map$set erros", {
  Map <- rgee::R6Map$new()
  expect_error(Map$setCenter("cesar"))
  expect_error(Map$setCenter(0, "cesar"))
  expect_error(Map$setCenter(0, 0, "cesar"))
  expect_error(Map$setZoom("cesar"))
})

## R6Map addLayer
test_that("Map$addLayer r6map", {
  Map <- rgee::R6Map$new()
  expect_error(Map$addLayer(ee$List(list(0))))
  geom_01 <- ee$Geometry$Point(c(0, 0))
  Map$addLayer(geom_01, list(width=5, color = "FFFFFF"))
  expect_s3_class(Map, "EarthEngineMap")
})

test_that("Map$centerObject_COG r6map", {
  # amazon
  resource <- "https://oin-hotosm.s3.amazonaws.com/56f9b5a963ebf4bc00074e70/0/56f9c2d42b67227a79b4faec.tif"
  visParams <- list(nodata = 0, expression = "B1*1+B2*4+B3*2", rescale = "0, 2000", colormap_name = "viridis")
  Map$centerObject(resource)
  Map$addLayer(resource, visParams = visParams, shown = TRUE)

  # google
  resource <- "https://storage.googleapis.com/pdd-stac/disasters/hurricane-harvey/0831/20170831_172754_101c_3B_AnalyticMS.tif"
  visParams <- list(nodata = 0, expression = "B3, B2, B1", rescale = "3000, 13500")
  Map$centerObject(resource)
  mcog <- Map$addLayer(resource, visParams = visParams, shown = TRUE)
  expect_s3_class(mcog, "EarthEngineMap")
})

