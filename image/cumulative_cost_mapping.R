library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# A rectangle representing Bangui, Central African Republic.
geometry <- ee$Geometry$Rectangle(list(18.5229, 4.3491, 18.5833, 4.4066))

# Create a source image where the geometry is 1, everything else is 0.
sources <- ee$Image()$toByte()$paint(geometry, 1)

# Mask the sources image with itself.
sources <- sources$updateMask(sources)

# The cost data is generated from classes in ESA/GLOBCOVER.
cover <- ee$Image("ESA/GLOBCOVER_L4_200901_200912_V2_3")$select(0)

# Classes 60, 80, 110, 140 have cost 1.
# Classes 40, 90, 120, 130, 170 have cost 2.
# Classes 50, 70, 150, 160 have cost 3.
cost <- cover$eq(60)$Or(cover$eq(80))$Or(cover$eq(110))$Or(cover$eq(140))$
  multiply(1)$add(
  cover$eq(40)$Or(cover$eq(90))$Or(cover$eq(120))$Or(cover$eq(130))$
    Or(cover$eq(170))$
    multiply(2)$add(
    cover$eq(50)$Or(cover$eq(70))$Or(cover$eq(150))$Or(cover$eq(160))$
      multiply(3)
  )
)

# Compute the cumulative cost to traverse the lAnd cover.
cumulativeCost <- cost$cumulativeCost(
  source = sources,
  maxDistance = 80 * 1000 # 80 kilometers
)

# Display the results
Map$setCenter(lon = 18.71, lat = 4.2)
Map$setZoom(zoom = 9)

Map$addLayer(
  eeObject = cover,
  visParams = list(),
  name = "Globcover"
) +
  Map$addLayer(
    eeObject = cumulativeCost,
    visParams = list(min = 0, max = 5e4),
    name = "accumulated cost"
  ) +
  Map$addLayer(
    eeObject = geometry,
    visParams = list(color = "FF0000"),
    name = "source geometry"
  )
