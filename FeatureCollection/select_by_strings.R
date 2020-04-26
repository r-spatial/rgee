library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Select states with "A" in its name
fc <- ee$FeatureCollection("TIGER/2018/States")$
  filter(ee$Filter$stringContains("STUSPS", "A"))

image <- ee$Image()$paint(fc, 0, 2)
Map$centerObject(fc, zoom = 3)
Map$addLayer(
  eeObject = image,
  visParams = list(palette = "FF0000"),
  name = "*A*"
)

# Select states its name starting with 'A'
fc <- ee$FeatureCollection("TIGER/2018/States")$
  filter(ee$Filter$stringStartsWith("STUSPS", "A"))

image <- ee$Image()$paint(fc, 0, 2)
Map$addLayer(
  eeObject = image,
  visParams = list(palette = "0000FF"),
  name = "A*"
)
