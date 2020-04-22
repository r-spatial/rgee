library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

image <- ee$Image("srtm90_v4")
vis_params <- list(min = 0, max = 3000)
Map$addLayer(image, vis_params, "SRTM")
