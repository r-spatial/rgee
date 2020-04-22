library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

dataset <- ee$Image("CSP/ERGo/1_0/US/physiography")
physiography <- dataset$select("constant")
physiographyVis <- list(
  min = 1100.0,
  max = 4220.0
)

Map$setCenter(-105.4248, 40.5242, 8)
Map$addLayer(physiography, physiographyVis, "Physiography")
