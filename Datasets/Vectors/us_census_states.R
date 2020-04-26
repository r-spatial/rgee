#' Display US States.
library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

fc <- ee$FeatureCollection("TIGER/2018/States")
image <- ee$Image()$paint(fc, 0, 2)

Map$setCenter(-99.844, 37.649, 5)
Map$addLayer(image, list(palette = "FF0000"), "TIGER/2018/States")
