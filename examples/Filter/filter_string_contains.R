library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

states = ee$FeatureCollection('TIGER/2018/States')

selected = states$filter(ee$Filter$stringContains("NAME", 'Dakota'))

Map$centerObject(selected, zoom =  5)
Map$addLayer(
  ee$Image()$paint(selected, 0, 2),
  list(palette = 'yellow'),
  'Selected'
)
