library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Use an elevation dataset and terrain functions to create
# a custom visualization of topography.

# Load a global elevation image.
elev <- ee$Image("USGS/GMTED2010")

# Zoom to an area of interest.
Map$setCenter(-121.069, 50.709, 6)

# Add the elevation to the map.
Map$addLayer(elev, name = "elev")

# Use the terrain algorithms to compute a hillshade with 8-bit values.
shade <- ee$Terrain$hillshade(elev)
Map$addLayer(shade, name = "hillshade", shown = FALSE)

# Create a "sea" variable to be used for cartographic purposes
sea <- elev$lte(0)
Map$addLayer(sea$mask(sea), list(palette = "000022"), "sea", FALSE)

# Create a custom elevation palette from hex strings.
elevationPalette <- c("006600", "002200", "fff700", "ab7634", "c4d0ff", "ffffff")

# Use these visualization parameters, customized by location.
visParams <- list(min = 1, max = 3000, palette = elevationPalette)

# Create a mosaic of the sea and the elevation data
visualized <- ee$ImageCollection(
  list(
    # Mask the elevation to get only land
    do.call(elev$mask(sea$Not())$visualize, visParams),
    # Use the sea mask directly to display sea.
    do.call(sea$mask(sea)$visualize, list(palette = "000022"))
  )
)$mosaic()

# Note that the visualization image doesn't require visualization parameters.
Map$addLayer(visualized, list(), "elev palette", FALSE)

# Convert the visualized elevation to HSV, first converting to [0, 1] data.
hsv <- visualized$divide(255)$rgbToHsv()
# Select only the hue and saturation bands.
hs <- hsv$select(0, 1)
# Convert the hillshade to [0, 1] data, as expected by the HSV algorithm.
v <- shade$divide(255)
# Create a visualization image by converting back to RGB from HSV.
# Note the cast to byte in order to export the image correctly.
rgb <- hs$addBands(v)$hsvToRgb()$multiply(255)$byte()
Map$addLayer(rgb, name = "styled")
