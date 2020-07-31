library(rgee)
ee_Initialize()

# Geometry
geom <- ee$Geometry$Rectangle(-10,-10,10,10)
Map$addLayer(geom)
ee_print(geom)

# Feature
feature <- ee$Feature(geom, list(rgee = "ee_print", data = TRUE))
ee_print(feature)

# FeatureCollection
featurecollection <- ee$FeatureCollection(feature)
ee_print(featurecollection)

# Image
srtm <- ee$Image("CGIAR/SRTM90_V4")
ee_print(srtm)

srtm_clip <- ee$Image("CGIAR/SRTM90_V4")$clip(geom)
srtm_metadata <- ee_print(srtm_clip)
srtm_metadata$img_bands_names

# ImageCollection
object <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
  filter(ee$Filter()$eq("WRS_PATH", 44))$
  filter(ee$Filter()$eq("WRS_ROW", 34))$
  filterDate("2014-03-01", "2014-08-01")$
  aside(ee_print)
