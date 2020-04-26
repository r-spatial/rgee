library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()


states = ee$FeatureCollection('TIGER/2018/States')

# Select states its name starting with 'Al'
selected = states$filter(ee$Filter$stringStartsWith('NAME', 'Al'))


Map$centerObject(selected, zoom =  3)
Map$addLayer(
  ee$Image()$paint(selected, 0, 2),
  list(palette = 'yellow'),
  'Selected'
)
