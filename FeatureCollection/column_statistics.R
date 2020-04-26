library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

fromFT <- ee$FeatureCollection("users/wqs/Pipestem/Pipestem_HUC10")
geom <- fromFT$geometry()


Map$centerObject(fromFT)
Map$addLayer(eeObject = fromFT) +
Map$addLayer(
  eeObject =  geom,
  name = "Watersheds"
)

print(fromFT$aggregate_stats("AreaSqKm")$getInfo())

total_area <- fromFT$reduceColumns(
  reducer = ee$Reducer$sum(),
  selectors = list("AreaSqKm")
  # weightSelectors = 'weight'
)$getInfo()

cat("Total area: ", total_area$sum)
