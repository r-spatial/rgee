library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

fromFT <- ee$FeatureCollection("users/wqs/Pipestem/Pipestem_HUC10")
centroid <- fromFT$geometry()$centroid()$coordinates()$getInfo()
lng <- centroid[1]
lat <- centroid[2]
print(sprintf("lng = %s, lat = %s", lng, lat))

count <- fromFT$size()$getInfo()
fromFT_list <- fromFT$toList(5)

map_list <- list()
for (r_index in seq_len(count)) {
  index <- r_index - 1
  feature <- ee$Feature(fromFT_list$get(index))
  name <- feature$get("system:index")$getInfo()
  fc <- fromFT$filter(ee$Filter$eq("system:index", name))
  Map$centerObject(fc, zoom = 9)
  map_list[[name]] <- Map$addLayer(fc, name = name)
}

Reduce(`+`, map_list)
