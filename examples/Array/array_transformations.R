library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# This function masks the input with a threshold on the simple cloud score.
cloudMask <- function(img) {
  cloudscore <- ee$Algorithms$Landsat$simpleCloudScore(img)$select("cloud")
  img$updateMask(cloudscore$lt(50))
}

# This function computes the predictors and the response from the input.
makeVariables <- function(img) {
  # Compute time of the image in fractional years relative to the Epoch.
  year <- ee$Image(img$date()$difference(ee$Date("1970-01-01"), "year"))
  # Compute the season in radians, one cycle per year.
  season <- year$multiply(2 * base::pi)
  # Return an image of the predictors followed by the response.
  img$select()$
    addBands(ee$Image(1))$
    addBands(year$rename("t"))$
    addBands(season$sin()$rename("sin"))$
    addBands(season$cos()$rename("cos"))$
    addBands(image$normalizedDifference()$rename("NDVI"))$
    toFloat()
}


# Load a Landsat 5 image collection.
collection <- ee$ImageCollection("LANDSAT/LT05/C01/T1_TOA")$
  filterDate("2008-04-01", "2010-04-01")$
  filterBounds(ee$Geometry$Point(-122.2627, 37.8735))$
  map(cloudMask)$
  select(c("B4", "B3"))$
  sort("system:time_start", TRUE)

# Define the axes of variation in the collection array.
imageAxis <- 0
bandAxis <- 1

# Convert the collection to an array.
array <- collection$map(makeVariables)$toArray()

# Check the length of the image axis (number of images).
arrayLength <- array$arrayLength(imageAxis)

# Update the mask to ensure that the number of images is greater than or
# equal to the number of predictors (the linear model is solveable).
array <- array$updateMask(arrayLength$gt(4))

# Get slices of the array according to positions along the band axis.
predictors <- array$arraySlice(bandAxis, 0, 4)
response <- array$arraySlice(bandAxis, 4)

# Compute coefficients the easiest way.
coefficients3 <- predictors$matrixSolve(response)

# Turn the results into a multi-band image.
coefficientsImage <- coefficients3$
  arrayProject(list(0))$
  arrayFlatten(list(c("constant", "trend", "sin", "cos")))

print(coefficientsImage$getInfo())
Map$setCenter(-122.2627, 37.8735, 10)
Map$addLayer(coefficientsImage, name = "coefficientsImage")
