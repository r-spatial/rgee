library(rgee)
library(raster)
ee_Initialize()
ee_user_info()

# Download five Landsat-8 more cloudless at Cusco Peru
roi <- ee$Geometry$Point(-73,-12)$buffer(1000)$bounds()

# ee_help(ee$ImageCollection$sort)
dataset <- ee$ImageCollection('LANDSAT/LC08/C01/T1_SR') %>%
  ee$ImageCollection$filterBounds(roi) %>%
  ee$ImageCollection$sort("CLOUD_COVER") %>%
  ee_get(0:4)

# View spatial objects interactively
Map$centerObject(dataset$first())
Map$addLayers(dataset)

# Get the system:time_start property of a EE ImageCollection
ee_get_date_ic(dataset)

# From Earth Engine to Local
ee_imagecollection_to_local(
  ic = dataset,
  region = roi,
  dsn = "l8_",
  scale = 100
)
