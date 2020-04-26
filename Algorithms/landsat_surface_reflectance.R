library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# You can access pre-computed surface reflectance images directly from the SR collections. For example, to load a Landsat 7 surface reflectance image, use:
srImage <- ee$Image("LANDSAT/LE07/C01/T1_SR/LE07_044034_19990707")

# The surface reflectance datasets for Collection 1 Landsat 4 through 7 are:
surfaceReflectanceL4 <- ee$ImageCollection("LANDSAT/LT04/C01/T1_SR")
surfaceReflectanceL5 <- ee$ImageCollection("LANDSAT/LT05/C01/T1_SR")
surfaceReflectanceL7 <- ee$ImageCollection("LANDSAT/LE07/C01/T1_SR")

Map$centerObject(srImage, zoom = 9)
Map$addLayer(
  srImage,
  list(bands = c("B4", "B3", "B2")),
  "Landsat Surface Reflectance"
)
