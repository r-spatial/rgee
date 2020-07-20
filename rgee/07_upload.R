library(stars)
library(rgee)
library(sf)

ee_Initialize(gcs = TRUE)
ee_user_info()

# 1. Convert a sf to ee$FeatureCollection (small sf objects)
x <- st_read(system.file("shape/nc.shp", package = "sf"))
ee_x <- sf_as_ee(x)

ee_x_bounds <- sf_as_ee(st_as_sfc(st_bbox(x)))
Map$centerObject(eeObject = ee_x_bounds)
Map$addLayer(ee_x)

# 2. Convert a sf to ee$FeatureCollection (large sf objects)
x <- st_read(system.file("shape/nc.shp", package = "sf"))
ee_x <- sf_as_ee(
  x = x,
  via = "gcs_to_asset",
  assetId = paste0(ee_get_assethome(),"/nc"),
  bucket = "rgee_dev",
  overwrite = TRUE
)
Map$addLayer(ee_x)

# 3. Convert a stars(raster) to ee$Image
tif <- system.file("tif/L7_ETMs.tif", package = "stars")
x <- read_stars(tif)
assetId <- sprintf("%s/%s",ee_get_assethome(),'stars_l7')
ee_raster_02 <- raster_as_ee(
  x = x,
  assetId = assetId,
  overwrite = TRUE,
  bucket = "rgee_dev"
)
Map$centerObject(ee_raster_02)
Map$addLayer(ee_raster_02)
