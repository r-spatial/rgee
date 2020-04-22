library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

dataset <- ee$ImageCollection("NRCan/CDEM")
elevation <- dataset$select("elevation")$mosaic()

elevationVis <- list(
  min = -50.0,
  max = 1500.0,
  palette = c("0905ff", "ffefc4", "ffffff")
)

Map$setCenter(-139.3643, 63.3213, 9)
Map$addLayer(elevation, elevationVis, "Elevation")
