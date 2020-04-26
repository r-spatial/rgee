library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

states = ee$FeatureCollection('TIGER/2018/States')

# Select states with its name ending with 'ia'
selected = states$filter(ee$Filter$stringEndsWith('NAME', 'ia'))

Map$centerObject(selected, 6)
Map$addLayer(
  ee$Image()$paint(selected, 0, 2),
  list(palette = 'yellow'),
  'Selected'
)
