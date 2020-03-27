library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# This function adds a time band to the image.
createTimeBand <- function(image) {
  image$addBands(image$metadata("system:time_start")$divide(1e18))
}

# createTimeBand = function(image) .
#   # Scale milliseconds by a large constant to avoid very small slope.
#   # in the linear regression output.
#   return image$addBands(image$metadata('system:time_start')$divide(1e18).

# Load the input image 'collection': projected climate data.
collection <- ee$ImageCollection("NASA/NEX-DCP30_ENSEMBLE_STATS")$
  filter(ee$Filter$eq("scenario", "rcp85"))$
  filterDate(ee$Date("2006-01-01"), ee$Date("2050-01-01"))$
  map(createTimeBand)

# Reduce the collection with the linear fit reducer.
# Independent variable are followed by dependent variables.
linearFit <- collection$select(c("system:time_start", "pr_mean"))$
  reduce(ee$Reducer$linearFit())

# Display the results.
Map$setCenter(-100.11, 40.38, 5)
Map$addLayer(
  eeObject = linearFit,
  visParams = list(min = 0, max = c(-0.9, 8e-5, 1), bands = c("scale", "offset", "scale")),
  name = "fit"
)
