library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

dataset <- ee$FeatureCollection("WCMC/WDPA/current/polygons")
visParams <- list(
  palette = c("2ed033", "5aff05", "67b9ff", "5844ff", "0a7618", "2c05ff"),
  min = 0.0,
  max = 1550000.0,
  opacity = 0.8
)

image <- ee$Image()$float()$paint(dataset, "REP_AREA")
Map$setCenter(41.104, -17.724, 6)
Map$addLayer(image, visParams, "WCMC/WDPA/current/polygons")

dataset <- ee$FeatureCollection("WCMC/WDPA/current/points")
styleParams <- list(
  color = "#4285F4",
  width = 1
)

protectedAreaPoints <- do.call(dataset$style, styleParams)
Map$setCenter(110.57, 0.88, 4)
Map$addLayer(protectedAreaPoints, {}, "WCMC/WDPA/current/points")
