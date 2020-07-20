library(rgee)
library(sf)

ee_Initialize(drive = TRUE)
ee_user_info()

# Load an Image Sentinel2 level-1C
ee_s2 <- ee$ImageCollection("COPERNICUS/S2")$
  filterDate("2016-01-01", "2016-01-31")$
  first()

# 1. From GEE to R
ee_s2$get("system:time_start")$getInfo() # bad
ee_s2$get("system:time_start") %>% eedate_to_rdate() # good!

# 2. From R to GEE
rdate_to_eedate("1981-01-01")
rdate_to_eedate(315532800000) # float number

# 3. Get the date of a ee$Image
ee_get_date_img(ee_s2)

# 4. Get the date of a ee$ImageCollection
nc <- st_read(system.file("shape/nc.shp", package = "sf")) %>%
  st_transform(4326) %>%
  sf_as_ee()

ee_s2 <- ee$ImageCollection("COPERNICUS/S2")$
  filterDate("2016-01-01", "2016-01-31")$
  filterBounds(nc)

ee_get_date_ic(ee_s2)
