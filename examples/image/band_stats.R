library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# get highest value
maxValue <- function(img, scale = 30) {
  img$reduceRegion(
    reducer = ee$Reducer$max(),
    geometry = img$geometry(),
    scale = scale,
    maxPixels = 1e9
  )
}

# get lowest value
minValue <- function(img, scale = 30) {
  img$reduceRegion(
    reducer = ee$Reducer$min(),
    geometry = img$geometry(),
    scale = scale,
    maxPixels = 1e9
  )
}

# get mean value
meanValue <- function(img, scale = 30) {
  img$reduceRegion(
    reducer = ee$Reducer$mean(),
    geometry = img$geometry(),
    scale = scale,
    maxPixels = 1e9
  )
}


# get standard deviation
stdValue <- function(img, scale = 30) {
  img$reduceRegion(
    reducer = ee$Reducer$stdDev(),
    geometry = img$geometry(),
    scale = scale,
    maxPixels = 1e9
  )
}

dataset <- ee$Image("USGS/NED")
dem <- dataset$select("elevation")
vis_params <- list(min = 0, max = 3000)
Map$setCenter(0, 0, 1)
Map$addLayer(dem, vis_params, "NED")

roi <- ee$Geometry$Polygon(list(
  c(-120.1820, 38.5348),
  c(-120.18204, 36.5488),
  c(-116.75431, 36.5488),
  c(-116.75431, 38.5348)
))

image <- dem$clip(roi)
Map$centerObject(image, 9)
Map$addLayer(image, vis_params, "DEM")

scale <- image$projection()$nominalScale()
cat("Resolution: ", scale$getInfo())

scale <- 30

cat("Minimum value: ", minValue(image, scale)$get("elevation")$getInfo())
cat("Maximum value: ", maxValue(image, scale)$get("elevation")$getInfo())
cat("Average value: ", meanValue(image, scale)$get("elevation")$getInfo())
cat("Standard deviation: ", stdValue(image, scale)$get("elevation")$getInfo())
