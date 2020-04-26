library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

dataset <- ee$Image("CSP/ERGo/1_0/Global/SRTM_CHILI")
srtmChili <- dataset$select("constant")
srtmChiliVis <- list(
  min = 0.0,
  max = 255.0
)

Map$setCenter(-105.8636, 40.3439, 11)
Map$addLayer(srtmChili, srtmChiliVis, "SRTM CHILI")
