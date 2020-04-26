library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

addArea <- function(feature) {
  feature$set(
    list(areaHa = feature$geometry()$area()$divide(100 * 100))
  )
}

# Load watersheds from a data table.
sheds <- ee$FeatureCollection("USGS/WBD/2017/HUC06")

# Map the area getting function over the FeatureCollection.
areaAdded <- sheds$map(addArea)

# Print the first feature from the collection with the added property.
ee_print(ee$Feature(areaAdded$first()))
