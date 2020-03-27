library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Compute the Principal Components of a Landsat 8 image.
# Load a landsat 8 image, select the bands of interest.
image <- ee$Image("LANDSAT/LC8_L1T/LC80440342014077LGN00")$
  select(c("B2", "B3", "B4", "B5", "B6", "B7", "B10", "B11"))

# Display the input imagery and the region in which to do the PCA.
region <- image$geometry()
visParams <- list(bands = c("B5", "B4", "B2"), min = 0, max = 20000)

Map$centerObject(ee$FeatureCollection(region), 10)
Map$addLayer(ee$Image()$paint(region, 0, 2), name = "Region") +
Map$addLayer(image, visParams, name = "Original Image")

# Set some information about the input to be used later.
scale <- 30
bandNames <- image$bandNames()

# Mean center the data to enable a faster covariance reducer
# and an SD stretch of the principal components.
meanDict <- image$reduceRegion(
  reducer = ee$Reducer$mean(),
  geometry = region,
  scale = scale,
  maxPixels = 1e9
)
means <- ee$Image$constant(meanDict$values(bandNames))
centered <- image$subtract(means)

# This helper function returns a list of new band names.
getNewBandNames <- function(prefix) {
  seq <- ee$List$sequence(1, bandNames$length())
  prefix_func <- function(b) {
    ee$String(prefix)$cat(ee$Number(b)$int()$format())
  }
  seq$map(ee_pyfunc(prefix_func))
}

# This function accepts mean centered imagery, a scale and
# a region in which to perform the analysis.  It returns the
# Principal Components (PC) in the region as a new image.
getPrincipalComponents <- function(centered, scale, region) {
  # Collapse the bands of the image into a 1D array per pixel.
  arrays <- centered$toArray()

  # Compute the covariance of the bands within the region.
  covar <- arrays$reduceRegion(
    reducer = ee$Reducer$centeredCovariance(),
    geometry = region,
    scale = scale,
    maxPixels = 1e9
  )

  # Get the 'array' covariance result and cast to an array.
  # This represents the band-to-band covariance within the region.
  covarArray <- ee$Array(covar$get("array"))

  # Perform an eigen analysis and slice apart the values and vectors.
  eigens <- covarArray$eigen()

  # This is a P-length vector of Eigenvalues.
  eigenValues <- eigens$slice(1, 0, 1)
  # This is a PxP matrix with eigenvectors in rows.
  eigenVectors <- eigens$slice(1, 1)

  # Convert the array image to 2D arrays for matrix computations.
  arrayImage <- arrays$toArray(1)

  # Left multiply the image array by the matrix of eigenvectors.
  principalComponents <- ee$Image(eigenVectors)$matrixMultiply(arrayImage)

  # Turn the square roots of the Eigenvalues into a P-band image.
  sdImage <- ee$Image(eigenValues$sqrt())$
    arrayProject(list(0))$
    arrayFlatten(list(getNewBandNames("sd")))

  # Turn the PCs into a P-band image, normalized by SD.
  principalComponents$
    arrayProject(list(0))$
    arrayFlatten(list(getNewBandNames("pc")))$
    divide(sdImage)
}

# Get the PCs at the specified scale and in the specified region
pcImage <- getPrincipalComponents(centered, scale, region)
Map$addLayer(pcImage$select(0), name = "Image")

pc_list <- list()
for (index in seq_len(bandNames$length()$getInfo())) {
  band <- pcImage$bandNames()$get(index - 1)$getInfo()
  map <- Map$addLayer(pcImage$select(band), list(min = -2, max = 2), band)
  pc_list[[index]] <- map
}

Reduce(`+`, pc_list)
