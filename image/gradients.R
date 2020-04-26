library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Load a Landsat 8 image and select the panchromatic band.
image <- ee$Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318")$select("B8")

# Compute the image gradient in the X and Y directions.
xyGrad <- image$gradient()

# Compute the magnitude of the gradient.
gradient <- xyGrad$select("x")$pow(2)$add(xyGrad$select("y")$pow(2))$sqrt()

# Compute the direction of the gradient.
direction <- xyGrad$select("y")$atan2(xyGrad$select("x"))

# Display the results.
Map$setCenter(lon = -122.054, lat = 37.7295)
Map$setZoom(zoom = 10)

Map$addLayer(
  eeObject = direction,
  visParams = list(min = -2, max = 2),
  name = "direction"
) +
  Map$addLayer(
    eeObject = gradient,
    visParams = list(min = -7, max = 7),
    name = "opened"
  )
