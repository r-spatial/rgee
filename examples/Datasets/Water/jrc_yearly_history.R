library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

dataset <- ee$ImageCollection("JRC/GSW1_1/YearlyHistory")$
  filter(ee$Filter$date("2015-01-01", "2015-12-31"))

waterClass <- dataset$select("waterClass")
waterClassVis <- list(
  min = 0.0,
  max = 3.0,
  palette = c("cccccc", "ffffff", "99d9ea", "0000ff")
)

Map$setCenter(59.414, 45.182, 7)
Map$addLayer(waterClass$mode(), waterClassVis, "Water Class")
