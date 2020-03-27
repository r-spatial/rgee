library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Visualization for WRI/GPPD/power_plants
# https:#code.earthengine.google.com/9efbd726e4a8ba9b8b56ba94f1267678

table <- ee$FeatureCollection("WRI/GPPD/power_plants")

# Get a color from a fuel
fuelColor <- ee$Dictionary(
  list(
    Coal = "000000",
    Oil = "593704",
    Gas = "BC80BD",
    Hydro = "0565A6",
    Nuclear = "E31A1C",
    Solar = "FF7F00",
    Waste = "6A3D9A",
    Wind = "5CA2D1",
    Geothermal = "FDBF6F",
    Biomass = "229A00"
  )
)

# List of fuels to add to the map
fuels <- c(
  "Coal", "Oil", "Gas",
  "Hydro", "Nuclear", "Solar",
  "Waste", "Wind", "Geothermal",
  "Biomass"
)

# /**
#  * Computes size from capacity and color from fuel type.
#  *
#  * @param {!ee.Geometry.Point} pt A point
#  * @return {!ee.Geometry.Point} Input point with added style dictionary.
#  */
addStyle <- function(pt) {
  size <- ee$Number(pt$get("capacitymw"))$sqrt()$divide(10)$add(2)
  color <- fuelColor$get(pt$get("fuel1"))
  pt$set("styleProperty", ee$Dictionary(list(pointSize = size, color = color)))
}

# Make a FeatureCollection out of the power plant data table
pp <- ee$FeatureCollection(table)$map(addStyle)
# print(pp$first())

fuels_d <- list()
fuel_style <- list(styleProperty = "styleProperty", neighborhood = 50)
Map$setCenter(0, 0, 1)
for (index in seq_along(fuels)) {
  fuel <- fuels[index]
  fuel_d <- do.call(pp$filter(ee$Filter$eq("fuel1", fuel))$style, fuel_style)
  map_object <- Map$addLayer(
    eeObject = fuel_d,
    visParams = {},
    name = fuel,
    shown = TRUE,
    opacity = 0.65
  )
  fuels_d[[index]] <- map_object
}

Reduce(`+`, fuels_d)
