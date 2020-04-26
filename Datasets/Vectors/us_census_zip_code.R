library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

dataset <- ee$FeatureCollection("TIGER/2010/ZCTA5")
visParams <- list(
  palette = c("black", "purple", "blue", "green", "yellow", "orange", "red"),
  min = 500000,
  max = 1000000000
)

zctaOutlines <- ee$Image()$float()$paint(
  featureCollection = dataset,
  color = "black",
  width = 1
)

image <- ee$Image()$float()$paint(dataset, "ALAND10")

Map$setCenter(-93.8008, 40.7177, 6)
Map$addLayer(image, visParams, "TIGER/2010/ZCTA5") +
Map$addLayer(zctaOutlines, {}, "borders")
