library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

dataset <- ee$FeatureCollection("TIGER/2010/Tracts_DP1")
visParams <- list(
  min = 0,
  max = 4000,
  opacity = 0.8
)

# Map$setCenter(-103.882, 43.036, 8)
image <- ee$Image()$float()$paint(dataset, "dp0010001")
Map$addLayer(image, visParams, "TIGER/2010/Tracts_DP1")
