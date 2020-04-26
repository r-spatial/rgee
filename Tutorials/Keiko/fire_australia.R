library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Credits to: Keiko Nomura, Senior Analyst, Space Intelligence Ltd
# Source: https://medium.com/google-earth/10-tips-for-becoming-an-earth-engine-expert-b11aad9e598b
# GEE JS: https://code.earthengine.google.com/?scriptPath=users%2Fnkeikon%2Fmedium%3Afire_australia

geometry <- ee$Geometry$Polygon(
  list(
    c(153.02512376008724, -28.052192238512877),
    c(153.02512376008724, -28.702237664294238),
    c(153.65683762727474, -28.702237664294238),
    c(153.65683762727474, -28.052192238512877)
  )
)

Map$centerObject(ee$FeatureCollection(geometry), zoom = 10)

# Use clear images from May and Dec 2019
imageMay <- ee$Image("COPERNICUS/S2_SR/20190506T235259_20190506T235253_T56JNP")
imageDec <- ee$Image("COPERNICUS/S2_SR/20191202T235239_20191202T235239_T56JNP")

Map$addLayer(
  eeObject = imageMay,
  visParams = list(
    bands = c("B4", "B3", "B2"),
    min = 0,
    max = 1800
  ),
  name = "May 2019 (True colours)"
) +
  Map$addLayer(
    eeObject = imageDec,
    visParams = list(
      bands = c("B4", "B3", "B2"),
      min = 0,
      max = 1800
    ),
    name = "Dec 2019 (True colours)"
  )

# Compute NDVI and use grey colour for areas with NDVI < 0.8 in May 2019
NDVI <- imageMay$normalizedDifference(c("B8", "B4"))$rename("NDVI")
grey <- imageMay$mask(NDVI$select("NDVI")$lt(0.8))

Map$addLayer(
  eeObject = grey,
  visParams = list(
    bands = c("B3", "B3", "B3"),
    min = 0,
    max = 1800,
    gamma = 1.5
  ),
  name = "grey (base)"
)

# Export as mosaic. Alternatively you can also use blend().
mosaicDec <- ee$ImageCollection(
  c(
    imageDec$visualize(
      bands = c("B4", "B3", "B2"),
      min = 0,
      max = 1800
    ),
    grey$visualize(
      bands = c("B3", "B3", "B3"),
      min = 0,
      max = 1800
    )
  )
)$mosaic()

mosaicMay <- ee$ImageCollection(c(
  imageMay$visualize(
    bands = c("B4", "B3", "B2"),
    min = 0,
    max = 1800
  ),
  grey$visualize(
    bands = c("B3", "B3", "B3"),
    min = 0,
    max = 1800
  )
))$mosaic()

# ee_image_to_drive(
#   image = mosaicMay,
#   description = 'May',
#   region = geometry,
#   crs = 'EPSG:3857',
#   scale = 10
# )

# ee_image_to_drive(
#   image = mosaicDec,
#   description = 'Dec',
#   region = geometry,
#   crs = 'EPSG:3857',
#   scale = 10
# )

# ============ #
#  Topography  #
# ============ #

# Add topography by computing a hillshade using the terrain algorithms
elev <- ee$Image("USGS/SRTMGL1_003")
shadeAll <- ee$Terrain$hillshade(elev)
shade <- shadeAll$mask(elev$gt(0)) # mask the sea

mayTR <- ee$ImageCollection(c(
  imageMay$visualize(
    bands = c("B4", "B3", "B2"),
    min = 0,
    max = 1800
  ),
  shade$visualize(
    bands = c("hillshade", "hillshade", "hillshade"),
    opacity = 0.2
  )
))$mosaic()

highVeg <- NDVI$gte(0.8)$visualize(
  min = 0,
  max = 1
)

Map$addLayer(mayTR$mask(highVeg), list(gamma = 0.8), "May (with topography)")

# Convert the visualized elevation to HSV, first converting to [0, 1] data.
hsv <- mayTR$divide(255)$rgbToHsv()
# Select only the hue and saturation bands.
hs <- hsv$select(0, 1)
# Convert the hillshade to [0, 1] data, as expected by the HSV algorithm.
v <- shade$divide(255)
# Create a visualization image by converting back to RGB from HSV.
# Note the cast to byte in order to export the image correctly.
rgb <- hs$addBands(v)$hsvToRgb()$multiply(255)$byte()

Map$addLayer(rgb$mask(highVeg), list(gamma = 0.5), "May (topography visualised)")

# Export the image
mayTRMosaic <- ee$ImageCollection(c(
  rgb$mask(highVeg)$visualize(gamma = 0.5),
  grey$visualize(
    bands = c("B3", "B3", "B3"),
    min = 0,
    max = 1800
  )
))$mosaic()

# ee_image_to_drive(
#   image = mayTRMosaic,
#   description = 'MayTerrain',
#   region = geometry,
#   crs = 'EPSG:3857',
#   scale = 10
# )

decTR <- ee$ImageCollection(c(
  imageDec$visualize(
    bands = c("B4", "B3", "B2"),
    min = 0,
    max = 1800
  ),
  shade$visualize(
    bands = c("hillshade", "hillshade", "hillshade"),
    opacity = 0.2
  )
))$mosaic()

Map$addLayer(decTR$mask(highVeg), list(gamma = 0.8), "Dec (with topography)")

# Convert the visualized elevation to HSV, first converting to [0, 1] data.
hsv <- decTR$divide(255)$rgbToHsv()
# Select only the hue and saturation bands.
hs <- hsv$select(0, 1)
# Convert the hillshade to [0, 1] data, as expected by the HSV algorithm.
v <- shade$divide(255)
# Create a visualization image by converting back to RGB from HSV.
# Note the cast to byte in order to export the image correctly.
rgb <- hs$addBands(v)$hsvToRgb()$multiply(255)$byte()

Map$addLayer(rgb$mask(highVeg), list(gamma = 0.5), "Dec (topography visualised)")

# Export the image
decTRMosaic <- ee$ImageCollection(c(
  rgb$mask(highVeg)$visualize(
    gamma = 0.5
  ),
  grey$visualize(
    bands = c("B3", "B3", "B3"),
    min = 0,
    max = 1800
  )
))$mosaic()

# ee_image_to_drive(
#   image = decTRMosaic,
#   description = 'DecTerrain',
#   region = geometry,
#   crs = 'EPSG:3857',
#   scale = 10
# )
