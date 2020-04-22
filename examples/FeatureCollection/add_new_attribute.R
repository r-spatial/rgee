library(rgee)
#ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# This function creates a new property that is the sum of two existing properties.
addField <- function(feature) {
  sum <- ee$Number(feature$get("property1"))$add(feature$get("property2"))
  feature$set(list(sum = sum))
}

# Create a FeatureCollection from a list of Features.
features <- ee$FeatureCollection(c(
  ee$Feature(
    ee$Geometry$Point(-122.4536, 37.7403),
    list(property1 = 100, property2 = 100)
  ),
  ee$Feature(
    ee$Geometry$Point(-118.2294, 34.039),
    list(property1 = 200, property2 = 200)
  )
))

# Map the function over the collection.
featureCollection <- features$map(addField)

# Print the entire FeatureCollection.
metadata <- ee_print(featureCollection, max_display = 3)

# Print a selected property of one Feature.
print(featureCollection$first()$get("sum")$getInfo())
