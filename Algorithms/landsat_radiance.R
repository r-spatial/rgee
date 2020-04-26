library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Load a raw Landsat scene and display it
raw <- ee$Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318")
viz <- list(bands = c("B4", "B3", "B2"), min = 6000, max = 12000)
Map$centerObject(raw, zoom = 10)
Map$addLayer(raw, viz, "raw")

# Convert the raw data to radiance$
radiance <- ee$Algorithms$Landsat$calibratedRadiance(raw)
viz <- list(bands = c("B4", "B3", "B2"), max = 90)
Map$addLayer(radiance, viz, "radiance")

# Convert the raw data to top-of-atmosphere reflectance.
toa <- ee$Algorithms$Landsat$TOA(raw)
viz <- list(bands = c("B4", "B3", "B2"), max = 0.2)
Map$addLayer(toa, viz, "toa reflectance")
