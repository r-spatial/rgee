library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

region <- ee$Geometry$Polygon(
  list(
    c(121.89674377441406, 11.91539248304918),
    c(121.98291778564453, 11.93218823174339),
    c(121.95236206054688, 12.020516709145957),
    c(121.86378679003906, 12.006748772470699)
  )
)

# Image Collection
collection <- ee$ImageCollection("COPERNICUS/S2")$
  filterDate("2017-03-01", "2017-04-04")$
  filterBounds(region)$
  filterMetadata("CLOUDY_PIXEL_PERCENTAGE", "less_than", 20)

print(collection$size()$getInfo())

first <- collection$first()

Map$centerObject(first, zoom = 10)
Map$addLayer(
  eeObject = first,
  visParams = list(
    min = 0,
    max = 3000,
    bands = c("B4", "B3", "B2")
  ),
  name = "TOA"
)
