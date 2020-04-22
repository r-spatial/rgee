library(rgee)
#ee_reattach() # reattach ee as a reserved word

ee_Initialize()

fromFT <- ee$FeatureCollection("users/wqs/Pipestem/Pipestem_HUC10")

# This function computes the feature's geometry area and adds it as a property.
addArea <- function(feature) {
  feature$set(list(areaHa = feature$geometry()$area()$divide(100 * 100)))
}

# Map the area getting function over the FeatureCollection.
areaAdded <- fromFT$map(addArea)

# Print the first feature from the collection with the added property.
first <- areaAdded$first()

ee_print(ee$Feature(first))
cat("areaHa: ", first$get("areaHa")$getInfo())
