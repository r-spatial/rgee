library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Load a Landsat 8 image.
image <- ee$Image("LANDSAT/LC08/C01/T1_TOA/LC08_044034_20140318")

# Compute the EVI using an expression.
evi <- image$expression(
  expression = "2.5 * ((NIR - RED) / (NIR + 6 * RED - 7.5 * BLUE + 1))",
  opt_map = list(
    NIR = image$select("B5"),
    RED = image$select("B4"),
    BLUE = image$select("B2")
  )
)

viz_params <- list(min = -1, max = 1, palette = c("FF0000", "00FF00"))
Map$centerObject(image, zoom = 9)
Map$addLayer(evi, viz_params, "EVI")
