library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

collection <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
  filter(ee$Filter$eq("WRS_PATH", 44))$
  filter(ee$Filter$eq("WRS_ROW", 34))$
  filterDate("2014-01-01", "2015-01-01")

# select by index from 0 to size-1
image <- ee$Image(collection$toList(collection$size())$get(0))
print(image$get("system:id")$getInfo())

Map$setCenter(-122.3578, 37.7726, 12)
Map$addLayer(image, list(bands = c("B4", "B3", "B2"), max = 0.3), "median")
