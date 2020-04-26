library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

dataset <- ee$Image("CSP/ERGo/1_0/Global/SRTM_topoDiversity")
srtmTopographicDiversity <- dataset$select("constant")
srtmTopographicDiversityVis <- list(
  min = 0.0,
  max = 1.0
)

Map$setCenter(-111.313, 39.724, 6)
Map$addLayer(
  srtmTopographicDiversity,
  srtmTopographicDiversityVis,
  "SRTM Topographic Diversity"
)
