library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Load a Landsat 8 image, select the panchromatic band$
image <- ee$Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318")$select("B8")

# Perform Canny edge detection and display the result$
canny <- ee$Algorithms$CannyEdgeDetector(image, threshold = 10, sigma = 1)

# Perform Hough transform of the Canny result and display$
hough <- ee$Algorithms$HoughTransform(canny, 256, 600, 100)

# Load a Landsat 8 image, select the panchromatic band$
image <- ee$Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318")$select("B8")

# Define a "fat" Gaussian kernel$
fat <- ee$Kernel$gaussian(
  radius = 3,
  sigma = 3,
  units = "pixels",
  normalize = T,
  magnitude = -1
)

# Define a "skinny" Gaussian kernel.
skinny <- ee$Kernel$gaussian(
  radius = 3,
  sigma = 1,
  units = "pixels",
  normalize = T
)

# Compute a difference-of-Gaussians (DOG) kernel.
dog <- fat$add(skinny)

# Compute the zero crossings of the second derivative, display.
zeroXings <- image$convolve(dog)$zeroCrossing()

Map$setCenter(lon = -122.054, lat = 37.7295)
Map$setZoom(zoom = 10)

Map$addLayer(
  eeObject = canny,
  visParams = list(min = 0, max = 1),
  name = "canny"
) +
  Map$addLayer(
    eeObject = hough,
    visParams = list(min = 0, max = 1),
    name = "hough"
  ) +
  Map$addLayer(
    eeObject = image,
    visParams = list(min = 0, max = 12000),
    name = "L_B8"
  ) +
  Map$addLayer(
    eeObject = zeroXings$updateMask(zeroXings),
    visParams = list(palette = "FF0000"),
    name = "zero crossings"
  )
