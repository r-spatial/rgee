library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Load the Sentinel-1 ImageCollection.
sentinel1 <- ee$ImageCollection("COPERNICUS/S1_GRD")$
  filterBounds(ee$Geometry$Point(-122.37383, 37.6193))

# Filter by metadata properties.
vh <- sentinel1$
  filter(ee$Filter$listContains("transmitterReceiverPolarisation", "VV"))$
  filter(ee$Filter$listContains("transmitterReceiverPolarisation", "VH"))$
  filter(ee$Filter$eq("instrumentMode", "IW"))

# Filter to get images from different look angles.
vhAscending <- vh$filter(ee$Filter$eq("orbitProperties_pass", "ASCENDING"))
vhDescending <- vh$filter(ee$Filter$eq("orbitProperties_pass", "DESCENDING"))

# Create a composite from means at different polarizations and look angles.
VH_Ascending_mean <- vhAscending$select("VH")$mean()
VV_Ascending_Descending_mean <- vhAscending$select("VV") %>%
  ee$ImageCollection$merge(vhDescending$select("VV")) %>%
  ee$ImageCollection$mean()
VH_Descending_mean <- vhDescending$select("VH")$mean()

composite <- ee$Image$cat(list(
  VH_Ascending_mean,
  VV_Ascending_Descending_mean,
  VH_Descending_mean
))$focal_median()

# Display as a composite of polarization and backscattering characteristics.
Map$setCenter(-122.37383, 37.6193, 10)
Map$addLayer(
  composite,
  list(min = c(-25, -20, -25), max = c(0, 10, 0)),
  "composite"
)
