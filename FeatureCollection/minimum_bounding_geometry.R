library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

HUC10 <- ee$FeatureCollection("USGS/WBD/2017/HUC10")
HUC08 <- ee$FeatureCollection("USGS/WBD/2017/HUC08")
roi <- HUC08$filter(ee$Filter$eq("name", "Pipestem"))

Map$centerObject(roi, zoom = 8)
Map$addLayer(roi, {}, "HUC8")

bound <- ee$Geometry(roi$geometry())$bounds()
Map$addLayer(
  eeObject = bound,
  visParams = list(palette = "red"),
  name = "Minimum bounding geometry"
)
