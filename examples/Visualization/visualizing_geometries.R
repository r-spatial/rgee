library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Create a geodesic polygon.
polygon <- ee$Geometry$Polygon(
  list(
    c(-5, 40), c(65, 40), c(65, 60), c(-5, 60), c(-5, 60)
  )
)

# Create a planar polygon.
planarPolygon <- ee$Geometry(polygon, {}, FALSE)
polygon <- ee$FeatureCollection(polygon)
planarPolygon <- ee$FeatureCollection(planarPolygon)

# Display the polygons by adding them to the map.
Map$centerObject(polygon, zoom = 2)
Map$addLayer(polygon, list(color = "FF0000"), "geodesic polygon") +
Map$addLayer(planarPolygon, list(color = "000000"), "planar polygon")
