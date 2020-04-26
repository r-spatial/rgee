library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# This example demonstrates the use of the Landsat 8 QA band to mask clouds.

# Function to mask clouds using the quality band of Landsat 8.
maskL8 <- function(image) {
  qa <- image$select("BQA")
  mask <- qa$bitwiseAnd(bitwShiftL(1, 4))$eq(0)
  image$updateMask(mask)
}

# Map the function over one year of Landsat 8 TOA data and take the median.
composite <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
  filterDate("2016-01-01", "2016-12-31")$
  map(maskL8)$
  median()

# Display the results in a cloudy place.
Map$setCenter(114.1689, 22.2986, 12)
Map$addLayer(composite, list(bands = c("B4", "B3", "B2"), max = 0.3), "Image")
