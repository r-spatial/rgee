library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

states <- ee$FeatureCollection("TIGER/2018/States")
# print(states$first()$getInfo())

# Select states with land area between 200,000 km2 and 300,000 km2
filter <- ee$Filter$rangeContains("ALAND", 200000000000, 300000000000)
selected <- states$filter(filter)
Map$centerObject(selected, zoom = 5)
Map$addLayer(
  ee$Image()$paint(selected, 0, 2),
  list(palette = "yellow"),
  "Selected"
)
