library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()
region <- ee$Geometry$Point(c(103.21996, 13.38761))

# Filter LANDSAT5 according Date and an ee$Geometry
l5 <- ee$ImageCollection("LANDSAT/LT5_L1T_TOA") %>%
  ee$ImageCollection$filterDate("2000-05-01", "2007-12-01") %>%
  ee$ImageCollection$filterBounds(region)

# Create a number ee$List where each element represent a month
months <- ee$List$sequence(1, 12)

# Function to Calculate a monthly composite
monthly_l5 <- function(m) {
  l5$filter(ee$Filter$calendarRange(m, m, "month")) %>%
    ee$ImageCollection$median() %>%
    ee$Image$select("B3", "B2", "B1")
}
l5_monthly <- months$map(ee_pyfunc(monthly_l5))

# Example: Display January and August median composite
l5_mean_jan <- ee$Image(l5_monthly$get(0))
l5_mean_aug <- ee$Image(l5_monthly$get(7))

## Vis parameters.
visparams <- list(
  bands = c("B3", "B2", "B1"),
  min = 0,
  max = 0.6,
  gamma = 1.4
)

Map$centerObject(region, zoom = 10)
Map$addLayer(l5_mean_jan, visparams, name = "Jan") +
Map$addLayer(l5_mean_aug, visparams, name = "Aug")
