library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Load a Landsat 8 image.
image <- ee$Image("LANDSAT/LC08/C01/T1_TOA/LC08_044034_20140318")

# Create NDVI and NDWI spectral indices.
ndvi <- image$normalizedDifference(c("B5", "B4"))
ndwi <- image$normalizedDifference(c("B3", "B5"))

# Create a binary layer using logical operations.
bare <- ndvi$lt(0.2)$And(ndwi$lt(0))

# Mask and display the binary layer.
Map$setCenter(lon = -122.3578, lat = 37.7726)
Map$setZoom(zoom = 12)

Map$addLayer(
  eeObject = bare$updateMask(bare),
  visParams = list(min = 0, max = 20),
  name = "bare"
)
