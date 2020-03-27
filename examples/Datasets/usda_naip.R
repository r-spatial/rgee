library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

roi <- ee$FeatureCollection("TIGER/2018/States")$
  filter(ee$Filter$eq("STUSPS", "ND"))

dataset <- ee$ImageCollection("USDA/NAIP/DOQQ")$
  filter(ee$Filter$date("2016-01-01", "2017-12-31"))$
  filterBounds(roi)

TrueColor <- dataset$select(c("N", "R", "G"))$mosaic()
TrueColorVis <- list(
  min = 0.0,
  max = 255.0
)

# Map$setCenter(-73.9958, 40.7278, 15)
Map$centerObject(roi, 8)
Map$addLayer(TrueColor, TrueColorVis, "True Color")
