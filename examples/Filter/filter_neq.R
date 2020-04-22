library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

states <- ee$FeatureCollection("TIGER/2018/States")

# Select all states except California
selected <- states$filter(ee$Filter$neq("NAME", "California"))

Map$centerObject(selected, zoom = 5)
Map$addLayer(
  ee$Image()$paint(selected, 0, 2),
  list(palette = "yellow"),
  "Selected"
)
