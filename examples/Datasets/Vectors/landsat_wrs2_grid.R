library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

dataset <- ee$FeatureCollection("projects/google/wrs2_descending")

empty <- ee$Image()$byte()

Map$setCenter(-78, 36, 5)
Map$addLayer(empty$paint(dataset, 0, 2), {}, "Landsat WRS-2 grid")
