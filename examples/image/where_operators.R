library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Load a cloudy Landsat 8 image.
image <- ee$Image("LANDSAT/LC08/C01/T1_TOA/LC08_044034_20130603")
Map$addLayer(
  eeObject = image,
  visParams = list(bands = c("B5", "B4", "B3"), min = 0, max = 0.5),
  name = "original image"
)

# Load another image to replace the cloudy pixels.
replacement <- ee$Image("LANDSAT/LC08/C01/T1_TOA/LC08_044034_20130416")

# Compute a cloud score band$
cloud <- ee$Algorithms$Landsat$simpleCloudScore(image)$select("cloud")

# Set cloudy pixels to the other image.
replaced <- image$where(cloud$gt(10), replacement)

# Display the result.
Map$centerObject(image, zoom = 9)
Map$addLayer(
  eeObject = replaced,
  visParams = list(
    bands = c("B5", "B4", "B3"),
    min = 0,
    max = 0.5
  ),
  name = "clouds replaced"
)
