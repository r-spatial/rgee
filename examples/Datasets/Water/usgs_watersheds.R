library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

dataset <- ee$FeatureCollection("USGS/WBD/2017/HUC02")
styleParams <- list(
  fillColor = "000070",
  color = "0000be",
  width = 3.0
)

regions <- do.call(dataset$style, styleParams)
Map$setCenter(-96.8, 40.43, 4)
Map$addLayer(regions, {}, "USGS/WBD/2017/HUC02")

dataset <- ee$FeatureCollection("USGS/WBD/2017/HUC04")
styleParams <- list(
  fillColor = "5885E3",
  color = "0000be",
  width = 3.0
)

subregions <- do.call(dataset$style, styleParams)
Map$setCenter(-110.904, 36.677, 7)
Map$addLayer(subregions, {}, "USGS/WBD/2017/HUC04")

dataset <- ee$FeatureCollection("USGS/WBD/2017/HUC06")
styleParams <- list(
  fillColor = "588593",
  color = "587193",
  width = 3.0
)

basins <- do.call(dataset$style, styleParams)
Map$setCenter(-96.8, 40.43, 7)
Map$addLayer(basins, {}, "USGS/WBD/2017/HUC06")


dataset <- ee$FeatureCollection("USGS/WBD/2017/HUC08")
styleParams <- list(
  fillColor = "2E8593",
  color = "587193",
  width = 2.0
)

subbasins <- do.call(dataset$style, styleParams)
Map$setCenter(-96.8, 40.43, 8)
Map$addLayer(subbasins, {}, "USGS/WBD/2017/HUC08")

dataset <- ee$FeatureCollection("USGS/WBD/2017/HUC10")
styleParams <- list(
  fillColor = "2E85BB",
  color = "2E5D7E",
  width = 1.0
)

watersheds <- do.call(dataset$style, styleParams)
Map$setCenter(-96.8, 40.43, 9)
Map$addLayer(watersheds, {}, "USGS/WBD/2017/HUC10")


dataset <- ee$FeatureCollection("USGS/WBD/2017/HUC12")
styleParams <- list(
  fillColor = "2E85BB",
  color = "2E5D7E",
  width = 0.1
)

subwatersheds <- do.call(dataset$style, styleParams)
Map$setCenter(-96.8, 40.43, 10)
Map$addLayer(subwatersheds, {}, "USGS/WBD/2017/HUC12")
