#' rgee Demo #3: Image preprocessing SENTINEL2
#' @author Cesar Aybar
library(cptcity)
library(raster)
library(stars)
library(rgee)
library(sf)

ee_Initialize(drive = TRUE)

# Define a region of interest with sf
ee_roi <- st_read(system.file("shape/nc.shp", package="sf")) %>%
  st_geometry() %>%
  sf_as_ee()

# Search into the Earth Engine’s public data archive
ee_search_dataset() %>%
  ee_search_tagstitle("sentinel", "sr",logical_operator = "AND") %>%
  '['(4,) %>%
  ee_search_display()

dataset <- ee$ImageCollection('COPERNICUS/S2_SR')

S2_clean <- function(img) {
  # Calculate the NDVI
  ndvi_values <- img$normalizedDifference(c("B8","B4"))

  # Extract the quality band
  img_qa <- img$select("SCL")

  # Create a mask considering: cloud shandows, medium&high clouds, cirrus
  cloud_mask <- img_qa$eq(list(3, 8, 9, 10))$reduce(ee$Reducer$sum())$gt(0)

  # Mask pixels with value zero.
  ndvi_values %>%
    ee$Image$updateMask(cloud_mask) %>%
    ee$Image$copyProperties(img, list("system:time_start"))
}

# Create a monthly composite
ndvi_composite <- dataset$
  filterDate('2019-01-01', '2019-12-31')$
  filter(ee$Filter$calendarRange(1, field = "month"))$
  map(S2_clean)$
  median()

# Display results
Map$setCenter(lon = -79,lat = 35,zoom = 9)
Map$addLayer(
  eeObject = ndvi_composite,
  visParams = list(
    min = 0.2 ,
    max = 0.7 ,
    palette = cpt("grass_ndvi", 10)
  )
) + Map$addLayer(ee_roi)

# Download raster
ee_raster <- ee_as_raster(
  image = ndvi_composite,
  region = ee_roi$geometry(),
  dsn = "/home/aybarpc01/ndvi_s2.tif",
  scale = 2000,
  via = "drive"
)

# ee_manage_cancel_all_running_task()
ndvi_mean_sf <- ee_extract(
  x = ndvi_composite,
  y = ee_roi,
  fun = ee$Reducer$mean(),
  scale = 2000,
  sf = TRUE
)
