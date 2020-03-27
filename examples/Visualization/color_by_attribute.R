library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

fc = ee$FeatureCollection('TIGER/2018/States')

print(fc$first()$getInfo())

# Use this empty image for paint().
empty = ee$Image()$byte()
palette = c('green', 'yellow', 'orange', 'red')

states = empty$paint(
  featureCollection = fc,
  color = 'ALAND'
)

Map$setCenter(-100.39886, 43.08957, 3)
Map$addLayer(states, list(palette = palette), 'US States')
