library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

dataset <- ee$Image("USGS/NLCD/NLCD2016")
landcover <- ee$Image(dataset$select("landcover"))

Map$setCenter(-95, 38, 5)
Map$addLayer(landcover$randomVisualizer(), name = "Landcover")
