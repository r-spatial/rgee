library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

states <- ee$FeatureCollection("TIGER/2018/States")
states <- states$map(function(f) f$set("STATEFP", ee$Number$parse(f$get("STATEFP"))))
state_image <- ee$Image()$float()$paint(states, "STATEFP")

visParams <- list(
  palette = c("purple", "blue", "green", "yellow", "orange", "red"),
  min = 0,
  max = 50,
  opacity = 0.8
)

counties <- ee$FeatureCollection("TIGER/2016/Counties")

image <- ee$Image()$paint(states, 0, 2)
Map$setCenter(-99.844, 37.649, 4)
Map$addLayer(image, visParams, "TIGER/2016/States") +
Map$addLayer(
  eeObject = ee$Image()$paint(counties, 0, 1),
  visParams = {},
  name = "TIGER/2016/Counties"
)

