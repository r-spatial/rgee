library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Collection.distance example.
# Computes the distance to the nearest feature in a collection.

# Construct a FeatureCollection from a list of geometries.
fc <- ee$FeatureCollection(
  list(
    ee$Geometry$Point(-72.944, 41.329),
    ee$Geometry$Point(-72.944, 41.334),
    ee$Geometry$Point(-72.944, 41.339),
    # The geometries do not need to be the same type.
    ee$Geometry$LineString(
      -72.93411, 41.30902,
      -72.93411, 41.31902,
      -72.94411, 41.31902
    )
  )
)

# Compute distance from the dfeatures, to a max of 1000 meters.
distance <- fc$distance(1000, 100)
ee_help(fc$distance)

Map$setCenter(-72.94, 41.32, 13)
Map$addLayer(
  eeObject = distance,
  visParams = list(
    min = 0,
    max = 1000,
    palette = c("yellow", "red")
  ),
  name = "distance"
) +
Map$addLayer(fc, name = "Features")
