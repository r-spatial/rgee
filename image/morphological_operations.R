library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Load a Landsat 8 image, select the NIR band, threshold, display.
image <- ee$Image("LANDSAT/LC08/C01/T1_TOA/LC08_044034_20140318")$select(4)$gt(0.2)

# Define a kernel.
kernel <- ee$Kernel$circle(radius = 1)

# Perform an erosion followed by a dilation, display.
opened <- image$focal_min(kernel = kernel, iterations = 2)$focal_max(kernel = kernel, iterations = 2)

Map$setCenter(lon = -122.1899, lat = 37.5010)
Map$setZoom(zoom = 13)

Map$addLayer(
  eeObject = image,
  visParams = list(min = 0, max = 1),
  name = "NIR threshold"
) +
  Map$addLayer(
    eeObject = opened,
    visParams = list(min = 0, max = 1),
    name = "opened"
  )
