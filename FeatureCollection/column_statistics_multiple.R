library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

fromFT <- ee$FeatureCollection("users/wqs/Pipestem/Pipestem_HUC10")
geom <- fromFT$geometry()


Map$addLayer(eeObject = fromFT) +
Map$addLayer(
  eeObject = geom,
  name = "Watersheds"
)

stats <- fromFT$reduceColumns(
  reducer = ee$Reducer$sum()$`repeat`(2),
  selectors = list("AreaSqKm", "AreaAcres")
  # weightSelectors: ['weight']
)$getInfo()

print(stats)
