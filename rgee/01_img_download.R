library(rgee)
library(raster)
ee_Initialize()
ee_user_info()

## 1. Download small images
dem <- ee$Image("WWF/HydroSHEDS/03CONDEM")
roi <- ee$Geometry$Point(-73,-12)$buffer(1000)$bounds()
# TIP: Use the dsn argument to specify where to save the raster.
dem_cusco_raster <- ee_as_raster(dem, roi)
dem_cusco_stars <- ee_as_stars(dem, roi)

## 2. Download large images
dem <- ee$Image("WWF/HydroSHEDS/03CONDEM")
roi <- ee$Geometry$Point(-73,-12)$buffer(10**5)$bounds()
# Map$centerObject(roi)
# Map$addLayer(dem$clip(roi))

# You need to upload your Google Drive credentials, rgee will make
# it automatically for you if you forgot it!
ee_Initialize(drive = TRUE)

dem_cusco_raster <- ee_as_raster(image = dem, region = roi, via = "drive")
dem_cusco_stars <- ee_as_stars(image = dem, region = roi, via = "drive")

# Clean a Google Drive folder
ee_clean_container(name = "rgee_backup", type = "drive")

