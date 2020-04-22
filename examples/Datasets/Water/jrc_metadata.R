library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

dataset <- ee$Image("JRC/GSW1_1/Metadata")
detectionsObservations <- dataset$select(
  c("detections", "valid_obs", "total_obs")
)

visParams <- list(
  min = 100.0,
  max = 900.0
)

Map$setCenter(4.72, -2.48, 2)
Map$addLayer(detectionsObservations, visParams, "Detections/Observations")
