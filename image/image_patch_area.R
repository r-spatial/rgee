library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

geometry <- ee$Geometry$Polygon(
  list(
    c(-121.53162002563477, 37.62442917942242),
    c(-121.53822898864746, 37.61871860390886),
    c(-121.53307914733887, 37.61144378319061),
    c(-121.5281867980957, 37.60784010375065),
    c(-121.52209281921387, 37.60586820524277),
    c(-121.51840209960938, 37.606344185530936),
    c(-121.51273727416992, 37.60777210812061),
    c(-121.50175094604492, 37.6082480762255),
    c(-121.49454116821289, 37.61239566936059),
    c(-121.49127960205078, 37.62136999709244),
    c(-121.49797439575195, 37.62667249978579),
    c(-121.5252685546875, 37.62653654290317)
  )
)

# Load a Landsat 8 image and display the thermal band.
image <- ee$Image("LANDSAT/LC8_L1T_TOA/LC80440342014077LGN00")$clip(geometry)
Map$setCenter(-121.51385307312012, 37.61767615130697, 14) # SF Bay
# Map$addLayer(image, list(bands = 'B10', min = 270, max = 310), 'LST')
# print(image)

# Threshold the thermal band to find "hot" objects.
hotspots <- image$select("B10")$gt(303)

# Mask "cold" pixels.
hotspots <- hotspots$mask(hotspots)
# Map$addLayer(hotspots, list(palette= 'FF0000'), 'hotspots')

# Compute the number of pixels in each patch.
patchsize <- hotspots$connectedPixelCount(100, FALSE)
Map$addLayer(patchsize, name = "patch size")
largePatches <- patchsize$gt(4)
largePatches <- largePatches$updateMask(largePatches)
Map$addLayer(largePatches, name = "patch size>4")

pixelAreaAllPatches <- hotspots$multiply(ee$Image$pixelArea())
pixelAreaLargePatch <- largePatches$multiply(ee$Image$pixelArea())
areaAllPathces <- pixelAreaAllPatches$reduceRegion(
  reducer = ee$Reducer$sum(),
  geometry = geometry
)
areaLargePatch <- pixelAreaLargePatch$reduceRegion(
  reducer = ee$Reducer$sum(),
  geometry = geometry
)
print(areaAllPathces$getInfo())
print(areaLargePatch$getInfo())
