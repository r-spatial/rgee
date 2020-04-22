library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Create two circular geometries.
poly1 <- ee$Geometry$Point(c(-50, 30))$buffer(1e6)
poly2 <- ee$Geometry$Point(c(-40, 30))$buffer(1e6)

# Display polygon 1 in red and polygon 2 in blue.
Map$setCenter(-45, 30)
Map$addLayer(poly1, list(color = "FF0000"), "poly1") +
Map$addLayer(poly2, list(color = "0000FF"), "poly2")

# Compute the intersection, display it in blue.
intersection <- poly1$intersection(poly2, ee$ErrorMargin(1))
Map$addLayer(intersection, list(color = "00FF00"), "intersection")

# Compute the union, display it in magenta.
union <- poly1$union(poly2, ee$ErrorMargin(1))
Map$addLayer(union, list(color = "FF00FF"), "union")

# Compute the difference, display in yellow.
diff1 <- poly1$difference(poly2, ee$ErrorMargin(1))
Map$addLayer(diff1, list(color = "FFFF00"), "diff1")

# Compute symmetric difference, display in black.
symDiff <- poly1$symmetricDifference(poly2, ee$ErrorMargin(1))
Map$addLayer(symDiff, list(color = "000000"), "symmetric difference")
