library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize(drive = TRUE)

fromFT = ee$FeatureCollection("users/wqs/Pipestem/Pipestem_HUC10")
Map$centerObject(fromFT)
Map$addLayer(fromFT)

# Method 01
sf_fromFT <- ee_as_sf(fromFT, via = 'drive')
plot(sf_fromFT)


# Method 02
taskParams = list(
  driveFolder =  'image',
  fileFormat = 'KML'   # CSV, KMZ, GeoJSON
)

# export all features in a FeatureCollection as one file
task <- ee$batch$Export$table(fromFT, 'export_fc', taskParams)
task$start()
ee_monitoring()
