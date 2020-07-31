#' rgee Demo #2: Image preprocessing LANDSAT
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
  ee_search_tags("L8","SR",logical_operator = "AND") %>%
  ee_search_display()

dataset <- ee$ImageCollection('LANDSAT/LC08/C01/T1_SR')


# Filter out poor quality pixels
getQABits <- function(image, qa) {
  # Convert binary (character) to decimal (little endian)
  qa <- sum(2^(which(rev(unlist(strsplit(as.character(qa), "")) == 1))-1))
  # Return a mask band image, giving the qa value.
  image$bitwiseAnd(qa)$lt(1)
}

# Using getQABits we construct a single-argument function 'mod13A2_clean'
l8_clean <- function(img) {
  # Calculate the NDVI
  ndvi_values <- img$normalizedDifference(c("B5","B4"))

  # Extract the quality band
  ndvi_qa <- img$select("pixel_qa")

  # Select pixels to mask
  quality_mask <- getQABits(ndvi_qa, "00000100001")

  # Mask pixels with value zero.
  ndvi_values %>%
    ee$Image$updateMask(quality_mask) %>%
    ee$Image$copyProperties(img, list("system:time_start"))
}


# Create a monthly composite
ndvi_composite <- dataset$
  filterDate('2016-01-01', '2016-12-31')$
  filter(ee$Filter$calendarRange(1, field = "month"))$
  map(l8_clean)$
  median()

# Display results
scale <- 0.0001
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
  dsn = "/home/aybarpc01/ndvi_l8.tif",
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
