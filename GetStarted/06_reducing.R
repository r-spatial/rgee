library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Load a Landsat 8 collection.
collection <- ee$ImageCollection("LANDSAT/LC08/C01/T1")$
  filterBounds(ee$Geometry$Point(-122.262, 37.8719))$
  filterDate("2014-01-01", "2014-12-31")$
  sort("CLOUD_COVER")

# Compute the median of each pixel for each band of the 5 least cloudy scenes.
median <- collection$limit(5)$reduce(ee$Reducer$median())

# Define visualization parameters in an object literal.
vizParams <- list(
  bands = c("B5_median", "B4_median", "B3_median"),
  min = 5000, max = 15000, gamma = 1.3
)

Map$addLayer(
  eeObject = median,
  visParams = vizParams,
  name = "Median image"
)
