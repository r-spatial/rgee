library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Load three NAIP quarter quads in the same location, different times.
naip2004_2012 <- ee$ImageCollection("USDA/NAIP/DOQQ")$
  filterBounds(ee$Geometry$Point(-71.08841, 42.39823))$
  filterDate("2004-07-01", "2012-12-31")$
  select(c("R", "G", "B"))

# Temporally composite the images with a maximum value function.
composite <- naip2004_2012$max()
Map$setCenter(lon = -71.12532, lat = 42.3712)
Map$setZoom(zoom = 12)

Map$addLayer(
  eeObject = composite,
  visParams = list(),
  name = "max value composite"
)

# Load four 2012 NAIP quarter quads, different locations.
naip2012 <- ee$ImageCollection("USDA/NAIP/DOQQ")$
  filterBounds(ee$Geometry$Rectangle(-71.17965, 42.35125, -71.08824, 42.40584))$
  filterDate("2012-01-01", "2012-12-31")

# Spatially mosaic the images in the collection and display.
mosaic <- naip2012$mosaic()
Map$addLayer(
  eeObject = mosaic,
  visParams = list(),
  name = "spatial mosaic"
)

# Load a NAIP quarter quad, display.
naip <- ee$Image("USDA/NAIP/DOQQ/m_4207148_nw_19_1_20120710")
Map$setCenter(lon = -71.0915, lat = 42.3443)
Map$setZoom(zoom = 14)

Map$addLayer(
  eeObject = naip,
  visParams = list(),
  name = "NAIP DOQQ"
)

# Create the NDVI and NDWI spectral indices.
ndvi <- naip$normalizedDifference(c("N", "R"))
ndwi <- naip$normalizedDifference(c("G", "N"))

# Create some binary images from thresholds on the indices.
# This threshold is designed to detect bare land.
bare1 <- ndvi$lt(0.2)$And(ndwi$lt(0.3))
# This detects bare land with lower sensitivity. It also detects shadows.
bare2 <- ndvi$lt(0.2)$And(ndwi$lt(0.8))

# Define visualization parameters for the spectral indices.
ndviViz <- list(min = -1, max = 1, palette = c("FF0000", "00FF00"))
ndwiViz <- list(min = 0.5, max = 1, palette = c("00FFFF", "0000FF"))

# Mask and mosaic visualization images.  The last layer is on top.
mosaic <- ee$ImageCollection(list(
  # NDWI > 0.5 is water.  Visualize it with a blue palette.
  ndwi$updateMask(ndwi$gte(0.5))$visualize(ndwiViz),
  # NDVI > 0.2 is vegetation.  Visualize it with a green palette.
  ndvi$updateMask(ndvi$gte(0.2))$visualize(ndviViz),
  # Visualize bare areas with shadow (bare2 but not bare1) as gray.
  bare2$updateMask(bare2$And(bare1$Not()))$visualize(list(palette = c("AAAAAA"))),
  # Visualize the other bare areas as white.
  bare1$updateMask(bare1)$visualize(list(palette = c("FFFFFF")))
))$mosaic()

Map$addLayer(
  eeObject = ndwi,
  visParams = list(),
  name = "Visualization mosaic"
)
