library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Load a Landsat 8 image.
image <- ee$Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318")

# Combine the mean and standard deviation reducers.
reducers <- ee$Reducer$mean()$combine(
  reducer2 = ee$Reducer$stdDev(),
  sharedInputs = TRUE
)

# Use the combined reducer to get the mean and SD of the image.
stats <- image$reduceRegion(
  reducer = reducers,
  bestEffort = TRUE
)

# Display the dictionary of band means and SDs.
print(stats$getInfo())
