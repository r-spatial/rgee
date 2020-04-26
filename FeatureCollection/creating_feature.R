library(rgee)
#ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Create an ee.Geometry.
polygon = ee$Geometry$Polygon(
  list(
    c(-35, -10),
    c(35, -10),
    c(35, 10),
    c(-35, 10),
    c(-35, -10)
  )
)

# Create a Feature from the Geometry.
polyFeature = ee$Feature(polygon, list('foo' = 42, 'bar' = 'tart'))
polyFeature$getInfo()

Map$centerObject(polyFeature)
Map$addLayer(polyFeature,name =  'feature')
