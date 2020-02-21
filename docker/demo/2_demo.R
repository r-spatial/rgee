#' NDVI, Mapping a Function over a Collection, Quality Mosaicking
#' Tutorial: https://developers.google.com/earth-engine/tutorial_api_06

# Spatial R packages ---------------------------------------------------------------
library(rgee)
library(sf)
library(cptcity)
library(mapview)
library(leaflet)
library(rnaturalearth)
library(reticulate)

ee_Initialize(drive = TRUE) # Initialize Google Earth Engine (Just One time)


# 1. Study area -----------------------------------------------------------
world_map <- ne_countries(returnclass = "sf")
world_map <- world_map[world_map$name == "Ecuador", ]
plot(world_map[1])
ee_Ecuador <- world_map %>%
  st_geometry() %>%
  sf_as_ee(check_ring_dir = TRUE)


# 2. Search into the Earth Engine Data Catalog ----------------------------
ee_dataset() %>%
  ee_search_type("ImageCollection") %>%
  ee_search_tagstitle("landsat", "toa", "l8", logical_operator = "AND") %>%
  "["(1, "id") -> l8_name

# 3. Call the EE ImageCollection ------------------------------------------
ic_l8 <- ee$ImageCollection(l8_name)$
  filterBounds(ee_Ecuador)$
  filterDate("2015-01-01", "2017-12-31")


# 4. Add NDVI band (FLOAT) into the ImageCollection -----------------------
addNDVI <- function(image) {
  ndvi <- image$normalizedDifference(c("B5", "B4"))$rename("NDVI")
  return(image$addBands(ndvi))
}


# 5. Quality mosaic vs Simple mean ----------------------------------------
vizparams <- list(max = 0.3, bands = c("B4", "B3", "B2"))

ic_l8_mean <- ic_l8$map(addNDVI)$mean()$clip(ee_Ecuador)
ic_l8_mosaic <- ic_l8$map(addNDVI)$qualityMosaic("NDVI")$clip(ee_Ecuador)

map_1 <- ee_map(ic_l8_mean, vizparams = vizparams, objname = "mean")
map_2 <- ee_map(ic_l8_mosaic, vizparams = vizparams, objname = "quality")
map_1 + map_2

# 6. Fast Download (< 5mb) ----------------------------------------------
#    Download EE thumbnail images and read them as stars objects
ee_Ecuador_bounds <- ee$Geometry(ee_Ecuador$bounds())
ecuador_stars <- ee_as_thumbnail(
  x = ic_l8_mosaic,
  region = ee_Ecuador_bounds,
  vizparams = vizparams
)
image(ecuador_stars, rgb = c(3, 2, 1))

# 6. Download using Google Chrome (> 5mb) ------------------------------------

# Resampling the image for a more faster download (this step could be omitted)
image_to_download <- ic_l8_mosaic$
  select(c("B4", "B3", "B2"))$
  reproject(crs = "EPSG:4326", scale = 2500)

# Passing from Earth Engine to Google Drive
task_img <- ee$batch$Export$image$toDrive(
  image = image_to_download,
  folder = "ecuador_mosaic",
  fileFormat = "GEOTIFF",
  fileNamePrefix = "ecuador_mosaic"
)
# ee_manage_cancel_all_running_taks()
task_img$start()
ee_monitoring() # Monitoring task progress (Optional)

# Passing from Google Drive to Hard disk
ecuador_stars <- ee_download_drive(task_img)
plot(ecuador_stars)
image(ecuador_stars, rgb = c(1, 2, 3))
