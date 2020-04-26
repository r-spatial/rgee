library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Load an image.
image <- ee$Image("LANDSAT/LC08/C01/T1_TOA/LC08_044034_20140318")

# Define the visualization parameters.
vizParams <- list(
  bands = c("B5", "B4", "B3"),
  min = 0,
  max = 0.5,
  gamma = c(0.95, 1.1, 1)
)


# Center the map and display the image.
Map$setCenter(-122.1899, 37.5010, 10) # San Francisco Bay
Map$addLayer(image, vizParams, "false color composite")
