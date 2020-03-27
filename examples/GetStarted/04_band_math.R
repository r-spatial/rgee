library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# This function gets NDVI from Landsat 5 imagery.
getNDVI <- function(image) {
  return(image$normalizedDifference(c("B4", "B3")))
}

# Load two Landsat 5 images, 20 years apart.
image1 <- ee$Image("LANDSAT/LT05/C01/T1_TOA/LT05_044034_19900604")
image2 <- ee$Image("LANDSAT/LT05/C01/T1_TOA/LT05_044034_20100611")

# Compute NDVI from the scenes.
ndvi1 <- getNDVI(image1)
ndvi2 <- getNDVI(image2)

# Compute the difference in NDVI.
ndviDifference <- ndvi2$subtract(ndvi1)

ndviParams <- list(palette = c(
  "#d73027", "#f46d43", "#fdae61", "#fee08b",
  "#d9ef8b", "#a6d96a", "#66bd63", "#1a9850"
))
ndwiParams <- list(min = -0.5, max = 0.5, palette = c("FF0000", "FFFFFF", "0000FF"))

Map$centerObject(ndvi1)
Map$addLayer(ndvi1, ndviParams, "NDVI 1") +
Map$addLayer(ndvi2, ndviParams, "NDVI 2") +
Map$addLayer(ndviDifference, ndwiParams, "NDVI difference")
