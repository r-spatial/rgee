library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

dataset <- ee$FeatureCollection("GLIMS/current")
visParams <- list(
  palette = c("gray", "cyan", "blue"),
  min = 0.0,
  max = 10.0,
  opacity = 0.8
)

image <- ee$Image()$float()$paint(dataset, "area")

Map$setCenter(-35.618, 66.743, 7)
Map$addLayer(image, visParams, "GLIMS/current")
