library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# LSIB: Large Scale International Boundary Polygons, Detailed
dataset <- ee$FeatureCollection("USDOS/LSIB/2013")
visParams <- list(
  palette = c("f5ff64", "b5ffb4", "beeaff", "ffc0e8", "8e8dff", "adadad"),
  min = 0.0,
  max = 894.0,
  opacity = 0.8
)

Map$setCenter(0, 0, 1)
image <- ee$Image()$float()$paint(dataset, "iso_num")
Map$addLayer(image, visParams, "USDOS/LSIB/2013")
