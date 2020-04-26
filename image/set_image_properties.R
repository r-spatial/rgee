library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

addDate <- function(image) {
  # parse date stored in 'system:index'
  date <- ee$Date(image$get("system:index"))
  str <- date$format("YYYY-MM-dd")
  image$set(list("Date" = str))
}

# point = ee.Geometry.Point(-122.262, 37.8719)
# start = ee.Date('2014-06-01')
# finish = ee.Date('2014-10-01')

# filteredCollection = ee.ImageCollection('LANDSAT/LC08/C01/T1') \
#     .filterBounds(point) \
#     .filterDate(start, finish) \
#     .sort('CLOUD_COVER', True)

filteredCollection <- ee$ImageCollection("users/sdavidcomer/L7maskedNDVIdated")

# Bring in image collection
# ndvi = ee.ImageCollection('users/sdavidcomer/L7maskedNDVIdated')

# Map addDate over image collection
result <- filteredCollection$map(addDate)
print(result$first()$get("Date")$getInfo())
