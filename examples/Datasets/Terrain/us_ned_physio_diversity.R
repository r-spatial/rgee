library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

dataset <- ee$Image("CSP/ERGo/1_0/US/physioDiversity")
physiographicDiversity <- dataset$select("b1")
physiographicDiversityVis <- list(
  min = 0.0,
  max = 1.0
)

Map$setCenter(-94.625, 39.825, 7)
Map$addLayer(
  physiographicDiversity,
  physiographicDiversityVis,
  "Physiographic Diversity"
)
