library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Load a cloudy Landsat scene and display it.
cloudy_scene <- ee$Image("LANDSAT/LC08/C01/T1_TOA/LC08_044034_20140926")
vizParams <- list(bands = list("B4", "B3", "B2"), max = 0.4)

Map$centerObject(cloudy_scene,zoom = 5)
Map$addLayer(cloudy_scene, vizParams, "TOA", FALSE)

# Add a cloud score band.  It is automatically called 'cloud'.
scored <- ee$Algorithms$Landsat$simpleCloudScore(cloudy_scene)

# Create a mask from the cloud score and combine it with the image mask.
mask <- scored$select("cloud")$lte(20)

# Apply the mask to the image and display the result.
masked <- cloudy_scene$updateMask(mask)
Map$addLayer(masked, list(bands = c("B4", "B3", "B2"), max = 0.4), "masked")

# Load a Landsat 8 composite and set the SENSOR_ID property.
mosaic <- ee$Image(ee$ImageCollection("LANDSAT/LC8_L1T_8DAY_TOA")$first())$
  set("SENSOR_ID", "OLI_TIRS")

# Cloud score the mosaic and display the result.
scored_mosaic <- ee$Algorithms$Landsat$simpleCloudScore(mosaic)
Map$addLayer(
  scored_mosaic,
  list(bands = c("B4", "B3", "B2"), max = 0.4),
  "TOA mosaic",
  FALSE
)
