library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

roi <- ee$Geometry$Point(c(-99.2182, 46.7824))

collection <- ee$ImageCollection("USDA/NAIP/DOQQ")$
  filterBounds(roi)$
  filter(ee$Filter$listContains("system:band_names", "N"))

print(collection$size()$getInfo())

first <- collection$first()
Map$centerObject(first, 13)
Map$addLayer(first, list(bands = c("N", "R", "G")), "NAIP")
