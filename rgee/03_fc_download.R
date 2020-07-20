library(rgee)
ee_Initialize(drive = TRUE)
ee_user_info()

# Region of interest
roi <- ee$Geometry$Point(c(-122.2575, 37.8795)) %>%
  ee$Geometry$buffer(10000)

## 1. Download a small FeatureCollections
blocks <- ee$FeatureCollection("TIGER/2010/Blocks")
subset <- blocks$filterBounds(roi)
sf_subset <- ee_as_sf(x = subset, maxFeatures = 10000)
plot(sf_subset["countyfp10"])


## 2. Download a Large FeatureCollections
region <- ee$Geometry$Rectangle(-119.224, 34.669, -99.536, 50.064)
# ee_help(ee$FeatureCollection$randomPoints)
ee_randomPoints <- ee$FeatureCollection$randomPoints(region, 50000)
sf_random <- ee_as_sf(x = ee_randomPoints, via = "drive")
