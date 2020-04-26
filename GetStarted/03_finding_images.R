library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

collection <- ee$ImageCollection("LANDSAT/LC08/C01/T1")

point <- ee$Geometry$Point(-122.262, 37.8719)
start <- ee$Date("2014-06-01")
finish <- ee$Date("2014-10-01")

filteredCollection <- ee$ImageCollection("LANDSAT/LC08/C01/T1")$
  filterBounds(point)$
  filterDate(start, finish)$
  sort("CLOUD_COVER", TRUE)

first <- filteredCollection$first()

# Define visualization parameters in an object literal.
vizParams <- list(
  bands = c("B5", "B4", "B3"),
  min = 5000,
  max = 15000,
  gamma = 1.3
)

Map$addLayer(first, vizParams, "Landsat 8 image")

# Load a feature collection.
featureCollection <- ee$FeatureCollection("TIGER/2016/States")

# Filter the collection.
filteredFC <- featureCollection$filter(ee$Filter$eq("NAME", "California"))

# Display the collection.
Map$addLayer(
  eeObject = filteredFC,
  visParams = list(palette = "red"),
  name = "California"
)
