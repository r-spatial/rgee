library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

dataset <- ee$Image("WWF/HydroSHEDS/03CONDEM")
elevation <- dataset$select("b1")
elevationVis <- list(
  min = -50.0,
  max = 3000.0,
  gamma = 2.0
)

Map$setCenter(-121.652, 38.022, 8)
Map$addLayer(elevation, elevationVis, "Elevation")

dataset <- ee$Image("WWF/HydroSHEDS/03VFDEM")
elevation <- dataset$select("b1")
elevationVis <- list(
  min = -50.0,
  max = 3000.0,
  gamma = 2.0
)

Map$setCenter(-121.652, 38.022, 8)
Map$addLayer(elevation, elevationVis, "Elevation Void Filled")
