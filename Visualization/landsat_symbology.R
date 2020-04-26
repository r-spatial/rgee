library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

geometry <- ee$Geometry$Point(c(-122.30287, 37.4411157))

landsat <- ee$ImageCollection("LANDSAT/LC08/C01/T1")$
  filterDate("2016-01-01", "2017-01-01")$
  filterBounds(geometry)

composite <- ee$Algorithms$Landsat$simpleComposite(
  collection = landsat,
  asFloat = TRUE
)

rgbVis <- list(bands = c("B4", "B3", "B2"), min = 0, max = 0.3)
nirVis <- list(bands = c("B5", "B4", "B3"), min = 0, max = c(0.5, 0.3, 0.3))
tempVis <- list(
  bands = "B10",
  min = 280,
  max = 310,
  palette = c("blue", "red", "orange", "yellow")
)

Map$centerObject(geometry, zoom = 10)
Map$addLayer(composite, rgbVis, "RGB") +
  Map$addLayer(composite, nirVis, "False Color") +
  Map$addLayer(composite, tempVis, "Thermal")
