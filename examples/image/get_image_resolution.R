library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

naip <- ee$Image("USDA/NAIP/DOQQ/m_3712213_sw_10_1_20140613")
Map$setCenter(-122.466123, 37.769833, 17)
Map$addLayer(naip, list(bands = c("N", "R", "G")), "NAIP")

naip_resolution <- naip$select("N")$projection()$nominalScale()
cat("NAIP resolution: ", naip_resolution$getInfo())

landsat <- ee$Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318")
landsat_resolution <- landsat$select("B1")$projection()$nominalScale()
cat("Landsat resolution: ", landsat_resolution$getInfo())
