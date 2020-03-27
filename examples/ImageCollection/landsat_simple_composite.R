library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Composite 6 months of Landsat 8.
# Note that the input to simpleComposite is raw data.
l8 <- ee$ImageCollection("LANDSAT/LC08/C01/T1")

# The asFloat parameter gives floating-point TOA output instead of
# the UINT8 outputs of the default simpleComposite()$
composite <- ee$Algorithms$Landsat$simpleComposite(
  collection = l8$filterDate("2015-1-1", "2015-7-1"),
  asFloat = TRUE
)

# Pick a spot with lots of clouds.
Map$setCenter(-47.6735, -0.6344, 12)
# Display a composite with a band combination chosen from:
# https:#landsat.usgs.gov/how-do-landsat-8-band-combinations-differ-landsat-7-or-landsat-5-satellite-data
geoviz <- list(bands = c("B6", "B5", "B4"), max = c(0.3, 0.4, 0.3))
Map$addLayer(composite, geoviz)
