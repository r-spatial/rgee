library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

image <- ee$Image("USDA/NAIP/DOQQ/m_3712213_sw_10_1_20140613")
Map$setCenter(-122.466123, 37.769833, 17)
Map$addLayer(image, list(bands = c("N", "R", "G")), "NAIP")

geometry <- image$geometry()
py_help(image$reduceRegions)
means <- image$reduceRegions(
  collection = geometry,
  reducer = ee$Reducer$mean()$forEachBand(image),
  scale = 10
)

print(means$getInfo())
