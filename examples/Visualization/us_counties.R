library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

dataset <- ee$FeatureCollection("TIGER/2018/Counties")
visParams <- list(
  palette = c("purple", "blue", "green", "yellow", "orange", "red"),
  min = 0,
  max = 50,
  opacity = 0.8
)

# Turn the strings into numbers
set_prop <- function(f) f$set("STATEFP", ee$Number$parse(f$get("STATEFP")))
dataset <- dataset$map(set_prop)

image <- ee$Image()$float()$paint(dataset, "STATEFP")
countyOutlines <- ee$Image()$float()$paint(
  featureCollection = dataset,
  color = "black",
  width = 1
)

Map$setCenter(-99.844, 37.649, 5)
Map$addLayer(image, visParams, "TIGER/2018/Counties") +
Map$addLayer(countyOutlines, {}, "county outlines")
# Map$addLayer(dataset, {}, 'for Inspector', False)
