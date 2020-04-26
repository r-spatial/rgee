library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

dataset <- ee$Image("CSP/ERGo/1_0/Global/SRTM_mTPI")
srtmMtpi <- dataset$select("elevation")
srtmMtpiVis <- list(
  min = -200.0,
  max = 200.0,
  palette = c("0b1eff", "4be450", "fffca4", "ffa011", "ff0000")
)
Map$setCenter(-105.8636, 40.3439, 11)
Map$addLayer(srtmMtpi, srtmMtpiVis, "SRTM mTPI")
