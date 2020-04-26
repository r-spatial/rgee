library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

image <- ee$Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318")

# Define visualization parameters in an object literal.
vizParams <- list(
  bands = c("B5", "B4", "B3"),
  min = 5000,
  max = 15000,
  gamma = 1.3
)

# Center the map on the image and display.
Map$centerObject(image, 9)
Map$addLayer(image, vizParams, "Landsat 8 False color")

extent <- image$geometry()
outline <- ee$Image()$paint(extent, 0, 2)
Map$addLayer(
  eeObject = outline,
  visParams = list("palette" = "yellow"),
  name = "Image extent"
)

coordinates <- extent$coordinates()
print(coordinates$getInfo())
