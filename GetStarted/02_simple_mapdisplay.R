library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Load an image.
image <- ee$Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318")

# Display the image.
Map$centerObject(image)
Map$addLayer(image, name = "Landsat 8 original image")

# Define visualization parameters in an object literal.
vizParams <- list(
  bands = c("B5", "B4", "B3"),
  min = 5000, max = 15000, gamma = 1.3
)

Map$addLayer(image, vizParams, "Landsat 8 False color")

# Use Map to add features and feature collections to the map. For example,
counties <- ee$FeatureCollection("TIGER/2016/Counties")

Map$addLayer(
  eeObject = counties,
  visParams = vizParams,
  name = "counties"
)
