library(rgee)
#ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Create a FeatureCollection from an Earth Engine Table.

# Load census roads.
roads = ee$FeatureCollection('TIGER/2016/Roads')

# Get only interstates.
interstates = roads$filter(ee$Filter$eq('rttyp', 'I'))

# Get only surface roads.
surfaceRoads = roads$filter(ee$Filter$eq('rttyp', 'M'))

# Display the roads in different colors.
Map$setCenter(238.16, 37.43, 11)
Map$addLayer(surfaceRoads, list('color' = 'black'), 'surface roads') +
Map$addLayer(interstates, list('color' = 'red'), 'interstates')

