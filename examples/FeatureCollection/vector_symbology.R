library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

fc = ee$FeatureCollection('TIGER/2018/States')

image = ee$Image()$paint(
  featureCollection = fc,
  color = 1,
  width = 3
)

Map$setCenter(-99.844, 37.649, zoom =  5)
Map$addLayer(
  eeObject = image,
  visParams =  list(palette = 'FF0000'),
  name =  'TIGER/2018/States')
