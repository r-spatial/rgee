library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

dataset <- ee$Image("USGS/NED")
elevation <- dataset$select("elevation")
elevationVis <- list(
  min = 0.0,
  max = 4000.0,
  gamma = 1.6
)

Map$setCenter(-100.55, 40.71, 5)
Map$addLayer(elevation, elevationVis, "Elevation")
