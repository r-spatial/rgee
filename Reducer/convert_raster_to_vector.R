library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Load a Japan boundary from the Large Scale International Boundary dataset.
japan <- ee$FeatureCollection("USDOS/LSIB_SIMPLE/2017")$
  filter(ee$Filter$eq("country_na", "Japan"))

# Load a 2012 nightlights image, clipped to the Japan border.
nl2012 <- ee$Image("NOAA/DMSP-OLS/NIGHTTIME_LIGHTS/F182012")$
  select("stable_lights")$
  clipToCollection(japan)

# Define arbitrary thresholds on the 6-bit nightlights image.
zones <- nl2012$gt(30)$add(nl2012$gt(55))$add(nl2012$gt(62))
zones <- zones$updateMask(zones$neq(0))

# Convert the zones of the thresholded nightlights to vectors.
vectors <- zones$addBands(nl2012)$reduceToVectors(
  geometry = japan,
  crs = nl2012$projection(),
  scale = 1000,
  geometryType = "polygon",
  eightConnected = FALSE,
  labelProperty = "zone",
  reducer = ee$Reducer$mean()
)

# Display the thresholds.
Map$setCenter(139.6225, 35.712, 9)
Map$addLayer(
  eeObject = zones,
  visParams = list(min = 1, max = 3, palette = c("0000FF", "00FF00", "FF0000")),
  name = "raster"
)

# Make a display image for the vectors, add it to the map.
display <- ee$Image(0)$updateMask(0)$paint(vectors, "000000", 3)
Map$addLayer(
  eeObject = display,
  visParams = list(palette = "000000"),
  name = "vectors"
)
