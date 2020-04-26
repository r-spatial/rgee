library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

dataset <- ee$Image("JRC/GSW1_1/GlobalSurfaceWater")
occurrence <- dataset$select("occurrence")
occurrenceVis <- list(
  min = 0.0,
  max = 100.0,
  palette = c("ffffff", "ffbbbb", "0000ff")
)

Map$setCenter(59.414, 45.182, 6)
Map$addLayer(occurrence, occurrenceVis, "Occurrence")
