library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Create a geodesic polygon.
polygon <- ee$Geometry$Polygon(
  list(
    c(-5, 40),
    c(65, 40),
    c(65, 60),
    c(-5, 60),
    c(-5, 60)
  )
)

# Compute a buffer of the polygon.
buffer <- polygon$buffer(0.1)

# Compute the centroid of the polygon.
centroid <- polygon$centroid()

Map$centerObject(buffer)
Map$addLayer(eeObject = buffer, name = "buffer") +
Map$addLayer(eeObject = centroid, list(color = "red"), name = "centroid")
