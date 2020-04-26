library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Load and display a Landsat TOA image.
image <- ee$Image("LANDSAT/LC08/C01/T1_TOA/LC08_044034_20140318")
Map$addLayer(
  eeObject = image,
  visParams = list(bands = c("B4", "B3", "B2"), max = 30000),
  name = "Landsat 8"
)

# Create an arbitrary rectangle as a region and display it.
region <- ee$Geometry$Rectangle(-122.2806, 37.1209, -122.0554, 37.2413)
Map$addLayer(
  eeObject = region,
  name = "Region"
)

# Get a dictionary of means in the region.  Keys are bandnames.
mean <- image$reduceRegion(
  reducer = ee$Reducer$mean(),
  geometry = region,
  scale = 30
)

print(mean$getInfo())
