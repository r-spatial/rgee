library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

states <- ee$FeatureCollection("TIGER/2018/States")
myfilter <- ee$Filter$inList(
  opt_leftField = "NAME",
  opt_rightValue = list("California", "Nevada", "Utah", "Arizona")
)
selected <- states$filter(myfilter)

Map$centerObject(selected, zoom = 6)
Map$addLayer(
  ee$Image()$paint(selected, 0, 2),
  list(palette = "yellow"),
  "Selected"
)
