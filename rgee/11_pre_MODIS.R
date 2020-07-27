#' rgee Demo #2: Image preprocessing MODIS
#' @author Cesar Aybar
#'
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
  ee_search_title("mod13") %>%
  ee_search_title("1km") %>%
  ee_search_display()

modis_ndvi <- ee$ImageCollection("MODIS/006/MOD13A2")


# Filter out poor quality pixels
getQABits <- function(image, qa) {
  # Convert binary (character) to decimal (little endian)
  qa <- sum(2^(which(rev(unlist(strsplit(as.character(qa), "")) == 1))-1))
  # Return a mask band image, giving the qa value.
  image$bitwiseAnd(qa)$lt(1)
}

# Using getQABits we construct a single-argument function 'mod13A2_clean'
mod13A2_clean <- function(img) {
  # Extract the NDVI band
  ndvi_values <- img$select("NDVI")

  # Extract the quality band
  ndvi_qa <- img$select("SummaryQA")

  # Select pixels to mask
  quality_mask <- getQABits(ndvi_qa, "11")

  # Mask pixels with value zero.
  ndvi_values$updateMask(quality_mask)
}

# Create a monthly composite
ndvi_composite <- modis_ndvi$
  filter(ee$Filter$date('2001-01-01', '2019-12-31'))$
  filter(ee$Filter$calendarRange(1, field = "month"))$
  map(mod13A2_clean)$
  median()

# Display results
scale <- 0.0001
Map$setCenter(lon = -79,lat = 35,zoom = 6)
Map$addLayer(
  eeObject = ndvi_composite,
  visParams = list(
    min = 0.2 / scale,
    max = 0.7 / scale,
    palette = cpt("grass_ndvi", 10)
  )
) + Map$addLayer(ee_roi)

# Download raster
ee_raster <- ee_as_raster(
  image = ndvi_composite,
  region = ee_roi$geometry(),
  dsn = "/home/aybarpc01/dd.tif",
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
