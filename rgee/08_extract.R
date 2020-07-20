library(tidyverse)
library(gganimate)
library(rgee)
library(sf)

ee_Initialize()
ee_user_info()

# Define a Image or ImageCollection: Terraclimate
terraclimate <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
  filterDate("2001-01-01", "2002-01-01")$
  map(function(x){
    date <- ee$Date(x$get("system:time_start"))$format('YYYY_MM_dd')
    name <- ee$String$cat("Terraclimate_pp_", date)
    x$select("pr")$reproject("EPSG:4326")$set("RGEE_NAME", name)
  })

# Define a geometry
nc <- st_read(
  dsn = system.file("shape/nc.shp", package = "sf"),
  stringsAsFactors = FALSE,
  quiet = TRUE
)

# Extract values
ee_nc_rain <- ee_extract(
  x = terraclimate,
  y = nc,
  scale = 250,
  fun = ee$Reducer$mean(),
  sf = FALSE
)

# gganimate
colnames(ee_nc_rain) <- sprintf("%02d", 1:12)
ee_nc_rain$name <- nc$NAME

ee_nc_rain %>%
  pivot_longer(-name, names_to = "month", values_to = "pr") %>%
  ggplot(aes(x = as.integer(month), y = pr, color = pr)) +
  geom_line(alpha = 0.8, size = 2) +
  xlab("Month") +
  ylab("Precipitation (mm)") +
  theme_minimal() +
  transition_states(name) +
  shadow_mark(size = 0.4, colour = "grey")

