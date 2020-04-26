#' Composite an ee$ImageCollection and clip it to a boundary
#' from a ee$FeatureCollection.
#'
#' See also: Filtered Seasonal Composite, which filters the
#' collection by bounds instead.

library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

fc <- ee$FeatureCollection("TIGER/2018/States")$
  filter(ee$Filter$eq("STUSPS", "MN"))

# Create a Landsat 7, median-pixel composite for Spring of 2000.
collection <- ee$ImageCollection("LE7_L1T")$
  filterDate("2000-05-01", "2000-10-31")$
  filterBounds(fc)
image1 <- collection$median()

# Map$addLayer(image1)
# # Clip to the output image to the California state boundary.
# # fc = (ee$FeatureCollection('ft:1fRY18cjsHzDgGiJiS2nnpUU3v9JPDc2HNaR7Xk8')
# #       $filter(ee$Filter()$eq('Name', 'Minnesota')))

image2 <- image1$clipToCollection(fc)

# Select the red, green and blue bands.
image <- image2$select("B4", "B3", "B2")
Map$setCenter(-93.7, 45, 5)
Map$addLayer(image, list(gain = c(1.4, 1.4, 1.1)), "Landsat 7")
