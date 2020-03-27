library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

img <- ee$Image("COPERNICUS/S2_SR/20191115T074201_20191115T075706_T37MBM")
ndvi <- img$normalizedDifference(c("B8", "B4"))
pal <- c("red", "yellow", "green")

Map$setCenter(36.9, -7.7, 12)
Map$addLayer(ndvi, list(min = 0, max = 0.8, palette = pal), "NDVI")
