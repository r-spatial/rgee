library(rgee)
ee_Initialize()

# Load a Landsat 8 ImageCollection for a single path-row.
collection <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
  filter(ee$Filter$eq("WRS_PATH", 44))$
  filter(ee$Filter$eq("WRS_ROW", 34))$
  filterDate("2014-03-01", "2014-09-01")
ee_print(collection)

# Get the number of images.
count <- collection$size()
cat("Count: ", count$getInfo())

# Get the date range of images in the collection.
range <- collection$reduceColumns(
  ee$Reducer$minMax(),
  list("system:time_start")
)

col_min <- eedate_to_rdate(range$get("min"))
col_max <- eedate_to_rdate(range$get("max"))
cat("Date range: ", as.character(col_min), as.character(col_max))

# Get statistics for a property of the images in the collection.
sunStats <- collection$aggregate_stats("SUN_ELEVATION")
cat("Sun elevation statistics: ")
sunStats$getInfo()

# Sort by a cloud cover property, get the least cloudy image.
image <- ee$Image(collection$sort("CLOUD_COVER")$first())
cat("Least cloudy image: ")
image$getInfo()

# Limit the collection to the 10 most recent images.
recent <- collection$sort("system:time_start", FALSE)$limit(10)
cat("Recent images: ")
recent$getInfo()
