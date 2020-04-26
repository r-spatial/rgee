library(rgee)
# ee_reattach() # reattach ee as a reserved word
ee_Initialize()

# An example finding linear features using the HoughTransform.
# Load an image and compute NDVI.
image <- ee$Image("LANDSAT/LC08/C01/T1_TOA/LC08_033032_20170719")
ndvi <- image$normalizedDifference(c("B5", "B4"))

# Apply a Canny edge detector.
canny <- ee$Algorithms$CannyEdgeDetector(
  image = ndvi,
  threshold = 0.4
)$multiply(255)

# Apply the Hough transform.
h <- ee$Algorithms$HoughTransform(
  image = canny,
  gridSize = 256,
  inputThreshold = 50,
  lineThreshold = 100
)

# Display.
Map$setCenter(-103.80140, 40.21729, 13)
Map$addLayer(
  eeObject = image,
  visParams = list(
    bands = c("B4", "B3", "B2"),
    max = 0.3
  ),
  name = "source_image"
) +
Map$addLayer(
  eeObject = canny$updateMask(canny),
  visParams = list(min = 0, max = 1, palette = "blue"),
  name = "canny"
) +
Map$addLayer(
  eeObject = h$updateMask(h),
  visParams = list(min = 0, max = 1, palette = "red"),
  name = "hough"
)
