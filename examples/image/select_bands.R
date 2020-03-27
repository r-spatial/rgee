library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Load an image.
image <- ee$Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318")
band345 <- image$select("B[3-5]")
bandNames <- band345$bandNames()
print(bandNames$getInfo())

# Define visualization parameters in an object literal.
vizParams <- list(
  bands = c("B5", "B4", "B3"),
  min = 5000,
  max = 15000,
  gamma = 1.3
)

# Center the map on the image and display.
Map$centerObject(image, zoom = 9)
Map$addLayer(
  eeObject = band345,
  visParams = vizParams,
  name = "Landsat 8 False color"
)
