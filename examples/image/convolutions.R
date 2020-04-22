library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Load and display an image.
image <- ee$Image("LANDSAT/LC08/C01/T1_TOA/LC08_044034_20140318")

# Define a boxcar or low-pass kernel.
# boxcar = ee$Kernel$square(list(
#   radius = 7, units = 'pixels', 'normalize' = T
# ))

boxcar <- ee$Kernel$square(7, "pixels", T)

# Smooth the image by convolving with the boxcar kernel.
smooth <- image$convolve(boxcar)

# Define a Laplacian, or edge-detection kernel.
laplacian <- ee$Kernel$laplacian8(1, F)

# Apply the edge-detection kernel.
edgy <- image$convolve(laplacian)

Map$setCenter(lon = -121.9785, lat = 37.8694)
Map$setZoom(zoom = 11)

Map$addLayer(
  eeObject = image,
  visParams = list(bands = c("B5", "B4", "B3"), max = 0.5),
  name = "input image"
) +
  Map$addLayer(
    eeObject = smooth,
    visParams = list(bands = c("B5", "B4", "B3"), max = 0.5),
    name = "smoothed"
  ) +
  Map$addLayer(
    eeObject = edgy,
    visParams = list(bands = c("B5", "B4", "B3"), max = 0.5),
    name = "edges"
  )
