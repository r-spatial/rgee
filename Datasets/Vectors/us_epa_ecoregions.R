library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

dataset <- ee$FeatureCollection("EPA/Ecoregions/2013/L3")
visParams <- list(
  palette = c("0a3b04", "1a9924", "15d812"),
  min = 23.0,
  max = 3.57e+11,
  opacity = 0.8
)

image <- ee$Image()$float()$paint(dataset, "shape_area")
Map$setCenter(-99.814, 40.166, 5)
Map$addLayer(image, visParams, "EPA/Ecoregions/2013/L3")

dataset <- ee$FeatureCollection("EPA/Ecoregions/2013/L4")
visParams <- list(
  palette = c("0a3b04", "1a9924", "15d812"),
  min = 0.0,
  max = 67800000000.0,
  opacity = 0.8
)

image <- ee$Image()$float()$paint(dataset, "shape_area")
Map$setCenter(-99.814, 40.166, 5)
Map$addLayer(image, visParams, "EPA/Ecoregions/2013/L4")
