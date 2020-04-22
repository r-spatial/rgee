library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# This function gets NDVI from Landsat 5 imagery.
getNDVI <- function(image) {
  image$normalizedDifference(c("B4", "B3"))
}

image1 <- ee$Image("LANDSAT/LT05/C01/T1_TOA/LT05_044034_19900604")

# Compute NDVI from the scene.
ndvi1 <- getNDVI(image1)

ndviParams <- list(palette = c(
  "#d73027", "#f46d43", "#fdae61",
  "#fee08b", "#d9ef8b", "#a6d96a",
  "#66bd63", "#1a9850"
))

Map$centerObject(image1, zoom = 10)
Map$addLayer(ndvi1, ndviParams, "NDVI")
