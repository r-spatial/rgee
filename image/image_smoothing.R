library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

image <- ee$Image("srtm90_v4")

smoothed <- image$reduceNeighborhood(
  reducer = ee$Reducer$mean(),
  kernel = ee$Kernel$square(3)
)

Map$setCenter(-112.40, 42.53, 10)
vis_params <- list(min = 0, max = 3000)
Map$addLayer(image, vis_params, "SRTM original") +
Map$addLayer(smoothed, vis_params, "SRTM smoothed")

Map$addLayer(ee$Terrain$hillshade(image), name = "Original hillshade") +
Map$addLayer(ee$Terrain$hillshade(smoothed), name = "Smoothed hillshade")
