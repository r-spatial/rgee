library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

waterSurface = ee$Image('JRC/GSW1_0/GlobalSurfaceWater')
waterChange = waterSurface$select('transition')
# Select Permanent Water Only:
Permanent_Water = 1 # value 1 represents pixels of permenant water, no change
waterMask = waterChange$eq(Permanent_Water) # Water mask boolean = 1 to detect whater bodies
# Map.setCenter(24.43874, 61.58173, 10)
# Map.addLayer(waterMask, {}, 'Water Mask')
# Map.centerObject(masked)
OnlyLakes = waterMask$updateMask(waterMask)

roi = ee$Geometry$Polygon(
  list(
    c(22.049560, 61.17121425),
    c(22.033081, 60.8330218),
    c(22.574157, 60.831683),
    c(22.5714, 61.171214))
)

classes = OnlyLakes$reduceToVectors(
  reducer = ee$Reducer$countEvery(),
  geometry = roi,
  scale = 30,
  maxPixels = 1e10
)

simpleClasses = classes$geometry()$simplify(50)

Map$centerObject(ee$FeatureCollection(roi), 10)
Map$addLayer(
  eeObject = ee$Image()$paint(classes, 0, 2),
  visParams = list(palette = 'red'),
  name =  "original") +
Map$addLayer(
  eeObject = ee$Image()$paint(simpleClasses, 0, 2),
  visParams = list(palette = 'blue'),
  name =  "simplified")
