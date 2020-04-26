library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

dataset <- ee$FeatureCollection("TIGER/2010/Blocks")
visParams <- list(
  min = 0.0,
  max = 700.0,
  palette = c("black", "brown", "yellow", "orange", "red")
)

image <- ee$Image()$float()$paint(dataset, "pop10")

Map$setCenter(-73.99172, 40.74101, 13)
Map$addLayer(image, visParams, "TIGER/2010/Blocks")
