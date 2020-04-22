library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Compute the trend of nighttime lights from DMSP.
# Add a band containing image date as years since 1990.
createTimeBand <- function(img) {
  year <- img$date()$difference(ee$Date("1990-01-01"), "year")
  ee$Image(year)$float()$addBands(img)
}

# Fit a linear trend to the nighttime lights collection.
collection <- ee$ImageCollection("NOAA/DMSP-OLS/CALIBRATED_LIGHTS_V4")$
  select("avg_vis")$
  map(createTimeBand)
fit <- collection$reduce(ee$Reducer$linearFit())

# Display a single image
Map$addLayer(
  eeObject = ee$Image(collection$select("avg_vis")$first()),
  visParams = list(min = 0, max = 63),
  name = "stable lights first asset"
)

# Display trend in red/blue, brightness in green.
Map$setCenter(30, 45, 4)
vizparams <- list(
  min = 0,
  max = c(0.18, 20, -0.18),
  bands = c("scale", "offset", "scale")
)

Map$addLayer(
  eeObject = fit,
  visParams = vizparams,
  name = "stable lights trend"
)
