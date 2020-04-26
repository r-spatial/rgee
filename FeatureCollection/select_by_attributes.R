library(rgee)
#ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Select North Dakota and South Dakota
fc = ee$FeatureCollection('TIGER/2018/States')$
  filter(
    ee$Filter$Or(
      ee$Filter$eq('STUSPS', 'ND'),
      ee$Filter$eq('STUSPS', 'SD')
    )
  )

image = ee$Image()$paint(fc, 0, 2)
Map$centerObject(fc, 6)
Map$addLayer(image, list(palette = 'FF0000'), 'TIGER/2018/States')
