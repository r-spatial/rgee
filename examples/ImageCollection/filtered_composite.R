#' Filter an image collection by date and region to
#' make a median composite.
#'
#' See also: Clipped composite, which crops the output image
#' instead of filtering the input collection.

library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Filter to only include images within the colorado and utah boundaries.
polygon <- ee$Geometry$Polygon(
  list(
    c(-109.05, 37.0),
    c(-102.05, 37.0),
    c(-102.05, 41.0),
    c(-109.05, 41.0),
    c(-111.05, 41.0),
    c(-111.05, 42.0),
    c(-114.05, 42.0),
    c(-114.05, 37.0),
    c(-109.05, 37.0)
  )
)

# Create a Landsat 7 composite for Spring of 2000, and filter by
# the bounds of the FeatureCollection.
collection <- ee$ImageCollection("LE7_L1T")$
  filterDate("2000-04-01", "2000-07-01")$
  filterBounds(polygon)

# Select the median pixel.
image1 <- collection$median()

# Select the red, green and blue bands.
image <- image1$select("B3", "B2", "B1")
Map$centerObject(polygon)
Map$addLayer(image, list(gain = c(1.4, 1.4, 1.1)))
