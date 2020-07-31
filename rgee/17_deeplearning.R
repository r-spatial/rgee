#' rgee Demo #8: Create a Deep Learning dataset CHIP (257x257)
#' @author Cesar Aybar

library(tidyverse)
library(raster)
library(rgee)
library(sf)
library(sp)

ee_Initialize()
SEED <- 100

ee_nc <- st_read(system.file("shape/nc.shp", package="sf")) %>%
  st_geometry %>%
  sf_as_ee %>%
  ee$FeatureCollection$geometry() %>%
  ee$Geometry$bounds()

# 1. Define the centroid of the chip
ee_random_points <- ee$FeatureCollection$randomPoints(
  region = ee_nc,
  points = 10,
  seed = SEED
)

random_points <- ee_as_sf(ee_random_points)

Map$centerObject(ee_nc)
Map$addLayer(ee_random_points)


# 2. Iterate for each chip
band_names <- c("B4","B3","B2","latitude","longitude")
scale <- 0.0001

for (index in seq_len(nrow(random_points))) {
  ## 2.1 Select a sfc point and convert it to ee$Geometry
  random_point <- st_geometry(random_points[index,])
  ee_random_point <- sf_as_ee(random_point)

  s2_img_array <- ee$ImageCollection("COPERNICUS/S2") %>%
    ee$ImageCollection$filterBounds(ee_random_point)%>% # Filter by space
    ee$ImageCollection$filterDate("2016-01-01", "2016-12-31") %>%  # Filter by time
    ee$ImageCollection$mosaic() %>% # Create a ee$Image
    ee$Image$addBands(ee$Image$pixelLonLat()) %>% # Add coordinates
    ee$Image$select(band_names) %>% # Select only bands specified in band_names
    ee$Image$reproject(crs = "EPSG:4326", scale = 10) %>% # Create a data-cube
    ee$Image$neighborhoodToArray(
      kernel = ee$Kernel$rectangle(128, 128, "pixels")
    ) %>% # from 1D to 2D (select pixel neighborhood)
    ee$Image$sampleRegions(ee$FeatureCollection(ee_random_point)) %>%
    ee$FeatureCollection$getInfo()

  # From nested list to tibble
  extract_fn <- function(x) as.numeric(unlist(s2_img_array$features[[1]]$properties[x]))
  image_as_df <- do.call(cbind,lapply(band_names, extract_fn))
  colnames(image_as_df) <- band_names
  image_as_tibble <- as_tibble(image_as_df)

  # From tibble to sp
  coordinates(image_as_tibble) <- ~longitude+latitude

  # From sp to raster
  sf_to_stack <- function(x) rasterFromXYZ(image_as_tibble[x])
  final_stack <- stack(lapply(names(image_as_tibble), sf_to_stack))
  crs(final_stack) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  final_stack <- final_stack*scale

  # Save results
  # plotRGB(final_stack,scale=1,stretch="hist")
  writeRaster(final_stack, sprintf("chip_%02d.tif", index))
}
