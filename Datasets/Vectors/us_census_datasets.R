library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()
Map$setCenter(-73.99172, 40.74101, 13)

dataset <- ee$FeatureCollection("TIGER/2010/Blocks")
visParams <- list(
  min = 0.0,
  max = 700.0,
  palette = c("black", "brown", "yellow", "orange", "red")
)

image <- ee$Image()$float()$paint(dataset, "pop10")
Map$addLayer(image, visParams, "TIGER/2010/Blocks")

dataset <- ee$FeatureCollection("TIGER/2010/Tracts_DP1")
visParams <- list(
  min = 0,
  max = 4000,
  opacity = 0.8
)

# Turn the strings into numbers
fun_set <- function(f) f$set("shape_area", ee$Number$parse(f$get("dp0010001")))
dataset <- dataset$map(fun_set)

# Map$setCenter(-103.882, 43.036, 8)
image <- ee$Image()$float()$paint(dataset, "dp0010001")
Map$addLayer(image, visParams, "TIGER/2010/Tracts_DP1")

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
Map$addLayer(image, visParams, "TIGER/2010/ZCTA5") +
Map$addLayer(
  eeObject = zctaOutlines,
  visParams = {},
  name = "borders"
)

dataset <- ee$FeatureCollection("TIGER/2016/Roads")
roads <- dataset$style(color = "#4285F4", width = 1)

Map$addLayer(
  eeObject = roads,
  visParams = {},
  name = "TIGER/2016/Roads"
)

dataset <- ee$FeatureCollection("TIGER/2018/Counties")
visParams <- list(
  palette = c("purple", "blue", "green", "yellow", "orange", "red"),
  min = 0,
  max = 50,
  opacity = 0.8
)

# Turn the strings into numbers
dataset <- dataset$map(function(f) f$set("STATEFP", ee$Number$parse(f$get("STATEFP"))))
image <- ee$Image()$float()$paint(dataset, "STATEFP")
countyOutlines <- ee$Image()$float()$paint(
  featureCollection = dataset,
  color = "black",
  width = 1
)

Map$setCenter(-99.844, 37.649, 5)
Map$addLayer(image, visParams, "TIGER/2018/Counties") +
Map$addLayer(
  eeObject = countyOutlines,
  visParams = {},
  name = "county outlines"
)

dataset <- ee$FeatureCollection("TIGER/2018/States")
visParams <- list(
  palette = c("purple", "blue", "green", "yellow", "orange", "red"),
  min = 500000000.0,
  max = 5e+11,
  opacity = 0.8
)

image <- ee$Image()$float()$paint(dataset, "ALAND")
Map$setCenter(-99.844, 37.649, 5)
Map$addLayer(image, visParams, "TIGER/2018/States")
# Map$addLayer(dataset, {}, 'for Inspector', False)
