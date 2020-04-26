library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

HUC10 <- ee$FeatureCollection("USGS/WBD/2017/HUC10")
HUC08 <- ee$FeatureCollection("USGS/WBD/2017/HUC08")
roi <- HUC08$filter(ee$Filter$eq("name", "Pipestem"))

Map$centerObject(roi, zoom = 8)
Map$addLayer(
  roi,
  {},
  "HUC08"
)

# select polygons intersecting the roi
roi2 <- HUC10$filter(
  ee$Filter$contains(
    leftValue = roi$geometry(),
    rightField = ".geo"
  )
)

Map$addLayer(
  eeObject = ee$Image()$paint(roi2, 0, 2),
  visParams = list(palette = "blue"),
  name = "HUC10"
)
