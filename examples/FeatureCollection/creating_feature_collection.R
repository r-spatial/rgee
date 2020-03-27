library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Make a list of Features.
features <- list(
  ee$Feature(
    ee$Geometry$Rectangle(30.01, 59.80, 30.59, 60.15),
    list(name = "Voronoi")
  ),
  ee$Feature(
    ee$Geometry$Point(-73.96, 40.781),
    list(name = "Thiessen")
  ),
  ee$Feature(
    ee$Geometry$Point(6.4806, 50.8012),
    list(name = "Dirichlet")
  )
)

# Create a FeatureCollection from the list and print it.
fromList <- ee$FeatureCollection(features)
ee_print(fromList)

# Create a FeatureCollection from a single geometry and print it.
fromGeom <- ee$FeatureCollection(
  ee$Geometry$Point(16.37, 48.225)
)
ee_print(fromGeom)
