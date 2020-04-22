library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Define a region in the redwood forest.
redwoods <- ee$Geometry$Rectangle(-124.0665, 41.0739, -123.934, 41.2029)

# Load input NAIP imagery and build a mosaic.
naipCollection <- ee$ImageCollection("USDA/NAIP/DOQQ")$
  filterBounds(redwoods)$
  filterDate("2012-01-01", "2012-12-31")

naip <- naipCollection$mosaic()

# Compute NDVI from the NAIP imagery.
naipNDVI <- naip$normalizedDifference(c("N", "R"))

# Compute standard deviation (SD) as texture of the NDVI.
texture <- naipNDVI$reduceNeighborhood(
  reducer = ee$Reducer$stdDev(),
  kernel = ee$Kernel$circle(7)
)

# Display the results.
Map$centerObject(ee$FeatureCollection(redwoods), zoom =  12)

Map$addLayer(
  eeObject = naip,
  visParams = list(),
  name = "NAIP input imagery"
) +
Map$addLayer(
  eeObject = naipNDVI,
  visParams = list(min = -1, max = 1, palette = c("FF0000", "00FF00")),
  name = "NDVI"
) +
Map$addLayer(
  eeObject = texture,
  visParams = list(min = 0, max = 0.3),
  name = "SD of NDVI"
)
