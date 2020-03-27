library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Load and filter the Sentinel-2 image collection.
collection <- ee$ImageCollection("COPERNICUS/S2")$
  filterDate("2016-01-01", "2016-12-31")$
  filterBounds(ee$Geometry$Point(c(-81.31, 29.90)))

# Reduce the collection.
extrema <- collection$reduce(ee$Reducer$minMax())
# print(extrema.getInfo())
min_image <- extrema$select(0)
max_image <- extrema$select(1)

Map$setCenter(lon = -81.31, lat = 29.90)
Map$setZoom(zoom = 10)

Map$addLayer(
  eeObject = min_image,
  visParams = list(min = 0, max = 10000),
  name = "Min image"
) +
Map$addLayer(
  eeObject = max_image,
  visParams = list(min = 0, max = 10000),
  name = "Max image"
)
