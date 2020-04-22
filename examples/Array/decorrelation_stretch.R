library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Decorrelation Stretch
dcs <- function(image, region, scale) {
  bandNames <- image$bandNames()
  # The axes are numbered, so to make the following code more
  # readable, give the axes names.
  imageAxis <- 0
  bandAxis <- 1

  # Compute the mean of each band in the region.
  means <- image$reduceRegion(ee$Reducer$mean(), region, scale)

  # Create a constant array image from the mean of each band.
  meansArray <- ee$Image(means$toArray())

  # Collapse the bands of the image into a 1D array per pixel,
  # with images along the first axis and bands along the second.
  arrays <- image$toArray()

  # Perform element-by-element subtraction, which centers the
  # distribution of each band within the region.
  centered <- arrays$subtract(meansArray)

  # Compute the covariance of the bands within the region.
  covar <- centered$reduceRegion(
    reducer = ee$Reducer$centeredCovariance(),
    geometry = region,
    scale = scale
  )

  # Get the 'array' result and cast to an array. Note this is a
  # single array, not one array per pixel, and represents the
  # band-to-band covariance within the region.
  covarArray <- ee$Array(covar$get("array"))

  # Perform an eigen analysis and slice apart the values and vectors.
  eigens <- covarArray$eigen()
  eigenValues <- eigens$slice(bandAxis, 0, 1)
  eigenVectors <- eigens$slice(bandAxis, 1)

  # Rotate by the eigenvectors, scale to a variance of 30, and rotate back.
  identity_mtx <- ee$Array$identity(bandNames$length())
  variance <- eigenValues$sqrt()$matrixToDiag()
  scaled <- identity_mtx$multiply(30)$divide(variance)
  rotation <- eigenVectors$transpose()$
    matrixMultiply(scaled)$
    matrixMultiply(eigenVectors)

  # Reshape the 1-D 'normalized' array, so we can left matrix multiply
  # with the rotation. This requires embedding it in 2-D space and
  # transposing.
  transposed <- centered$arrayRepeat(bandAxis, 1)$arrayTranspose()

  # Convert rotated results to 3 RGB bands, and shift the mean to 127.
  transposed$matrixMultiply(ee$Image(rotation))$
    arrayProject(list(bandAxis))$
    arrayFlatten(list(bandNames))$
    add(127)$
    byte()
}

image <- ee$Image("MODIS/006/MCD43A4/2002_07_04")

Map$setCenter(-52.09717, -7.03548, 6)

# View the original bland image.
rgb <- c(0, 3, 2)
Map$addLayer(image$select(rgb), list(min = -100, max = 2000), "Original Image")

# Stretch the values within an interesting region.
region <- ee$Geometry$Rectangle(-57.04651, -8.91823, -47.24121, -5.13531)
Map$addLayer(dcs(image, region, 1000)$select(rgb), name = "DCS Image") +
Map$addLayer(ee$Image()$paint(region, 0, 2), name = "Region")
