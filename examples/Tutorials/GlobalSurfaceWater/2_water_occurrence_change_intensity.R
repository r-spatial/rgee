library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

###############################
# Asset List
###############################

gsw = ee$Image('JRC/GSW1_1/GlobalSurfaceWater')
occurrence = gsw$select('occurrence')
change = gsw$select("change_abs")
roi = ee$Geometry$Polygon(
  list(
    c(-74.17213, -8.65569),
    c(-74.17419, -8.39222),
    c(-74.38362, -8.36980),
    c(-74.43031, -8.61293)
  )
)

###############################
# Constants
###############################

VIS_OCCURRENCE = list(
  min = 0,
  max = 100,
  palette = c('red', 'blue')
)

VIS_CHANGE = list(
  min = -50,
  max = 50,
  palette = c('red', 'black', 'limegreen')
)

VIS_WATER_MASK = list(
  palette = c('white', 'black')
)

###############################
# Calculations
###############################

# Create a water mask layer, and set the image mask so that non-water areas are transparent.
water_mask = occurrence$gt(90)$mask(1)

###############################
# Initialize Map Location
###############################

# Uncomment one of the following statements to center the map on
# a particular location.
# Map$setCenter(-90.162, 29.8597, 10)   # New Orleans, USA
# Map$setCenter(-114.9774, 31.9254, 10) # Mouth of the Colorado River, Mexico
# Map$setCenter(-111.1871, 37.0963, 11) # Lake Powell, USA
# Map$setCenter(149.412, -35.0789, 11)  # Lake George, Australia
# Map$setCenter(105.26, 11.2134, 9)     # Mekong River Basin, SouthEast Asia
# Map$setCenter(90.6743, 22.7382, 10)   # Meghna River, Bangladesh
# Map$setCenter(81.2714, 16.5079, 11)   # Godavari River Basin Irrigation Project, India
# Map$setCenter(14.7035, 52.0985, 12)   # River Oder, Germany & Poland
# Map$setCenter(-59.1696, -33.8111, 9)  # Buenos Aires, Argentina\
Map$setCenter(-74.4557, -8.4289, 11)  # Ucayali River, Peru

###############################
# Map Layers
###############################

Map$addLayer(water_mask, VIS_WATER_MASK, '90% occurrence water mask', FALSE) +
Map$addLayer(occurrence$updateMask(occurrence$divide(100)),  VIS_OCCURRENCE, "Water Occurrence (1984-2015)") +
Map$addLayer(change, VIS_CHANGE,'occurrence change intensity')
