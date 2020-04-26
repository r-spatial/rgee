library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

roi <- ee$Geometry$Polygon(
  list(
    c(-73.9989, 40.74560),
    c(-73.9989, 40.74053),
    c(-73.9874, 40.74053),
    c(-73.9874, 40.74560)
  )
)

fc <- ee$FeatureCollection('TIGER/2016/Roads')$filterBounds(roi)
clipped <- fc$map(function(x) x$intersection(roi))

Map$centerObject(roi)
Map$addLayer(eeObject = ee$FeatureCollection(roi)) +
Map$addLayer(
  eeObject = roi,
  visParams = list(palette = 'yellow'),
  name = 'ROI'
)

Map$centerObject(clipped)
Map$addLayer(
  eeObject = clipped,
  visParams = list(palette = 'red'),
  name = 'clipped') +
Map$addLayer(
  eeObject = fc,
  name = 'Census roads'
)
