library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

dataset <- ee$ImageCollection("USDA/NASS/CDL")$
  filter(ee$Filter$date("2017-01-01", "2018-12-31"))$
  first()

cropLandcover <- dataset$select("cropland")
Map$setCenter(-100.55, 40.71, 4)
Map$addLayer(cropLandcover, {}, "Crop Landcover")
