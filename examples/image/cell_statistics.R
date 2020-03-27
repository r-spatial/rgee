library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Load an image and select some bands of interest.
image <- ee$Image("LANDSAT/LC8_L1T/LC80440342014077LGN00")$
  select(c("B4", "B3", "B2"))

# Reduce the image to get a one-band maximum value image.
maxValue <- image$reduce(ee$Reducer$max())

# Display the result.
Map$centerObject(image, 10)
Map$addLayer(maxValue, list(max = 13000), "Maximum value image")
