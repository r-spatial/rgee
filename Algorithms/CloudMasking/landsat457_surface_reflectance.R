library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# This example demonstrates the use of the Landsat 4, 5 or 7
# surface reflectance QA band to mask clouds.
cloudMaskL457 <- function(image) {
  qa <- image$select("pixel_qa")
  # If the cloud bit (5) is set and the cloud confidence (7) is high
  # or the cloud shadow bit is set (3), then it's a bad pixel.
  cloud <- qa$bitwiseAnd(bitwShiftL(1, 5))$
    And(qa$bitwiseAnd(bitwShiftL(1, 7)))$
    Or(qa$bitwiseAnd(bitwShiftL(1, 3)))
  # Remove edge pixels that don't occur in all bands
  mask2 <- image$mask()$reduce(ee$Reducer$min())
  image$updateMask(cloud$Not())$updateMask(mask2)
}

# Map the function over the collection and take the median.
collection <- ee$ImageCollection("LANDSAT/LT05/C01/T1_SR")$
  filterDate("2010-04-01", "2010-07-30")

composite <- collection$map(cloudMaskL457)$median()

# Display the results in a cloudy place.
Map$setCenter(-6.2622, 53.3473, 12)
Map$addLayer(composite, list(bands = c("B3", "B2", "B1"), min = 0, max = 0.6))
