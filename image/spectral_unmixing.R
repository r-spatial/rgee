library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Load a Landsat 5 image and select the bands we want to unmix.
bands <- c("B1", "B2", "B3", "B4", "B5", "B6", "B7")

image <- ee$Image("LANDSAT/LT05/C01/T1/LT05_044034_20080214")$select(bands)

# Define spectral endmembers.
urban <- c(88, 42, 48, 38, 86, 115, 59)
veg <- c(50, 21, 20, 35, 50, 110, 23)
water <- c(51, 20, 14, 9, 7, 116, 4)

# Unmix the image.
fractions <- image$unmix(list(urban, veg, water))

Map$setCenter(lon = -122.1899, lat = 37.5010) # San Francisco Bay
Map$setZoom(zoom = 10)

Map$addLayer(
  eeObject = image,
  visParams = list(min = 0, max = 128, bands = c("B4", "B3", "B2")),
  name = "image"
) +
Map$addLayer(
  eeObject = fractions,
  visParams = list(min = 0, max = 2),
  name = "unmixed"
)
