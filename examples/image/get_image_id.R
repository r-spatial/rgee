library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# This function masks the input with a threshold on the simple cloud score$
cloudMask <- function(img) {
  cloudscore <- ee$Algorithms$Landsat$simpleCloudScore(img)$select("cloud")
  img$updateMask(cloudscore$lt(50))
}

# Load a Landsat 5 image collection.
collection <- ee$ImageCollection("LANDSAT/LT5_L1T_TOA")$
  filterDate("2008-04-01", "2010-04-01")$
  filterBounds(ee$Geometry$Point(-122.2627, 37.8735))$
  map(cloudMask)$
  select(c("B4", "B3"))$
  sort("system:time_start", TRUE) # Sort the collection in chronological order.

print(collection$size()$getInfo())

first <- collection$first()
propertyNames <- first$propertyNames()
print(propertyNames$getInfo())

uid <- first$get("system:id")
print(uid$getInfo())
