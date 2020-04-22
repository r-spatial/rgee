library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

areaDiff <- function(feature) {
  area <- feature$geometry()$area()$divide(1000 * 1000)
  # Compute the differece between computed area and the area property.
  diff <- area$subtract(ee$Number$parse(feature$get("areasqkm")))
  # Return the feature with the squared difference set to the 'diff' property.
  feature$set("diff", diff$pow(2))
}

# Load watersheds from a data table and filter to the continental US.
sheds <- ee$FeatureCollection("USGS/WBD/2017/HUC06")$
  filterBounds(ee$Geometry$Rectangle(-127.18, 19.39, -62.75, 51.29))

# Calculate RMSE for population of difference pairs.
rmse <- ee$Number(
  # Map the difference function over the collection.
  sheds$map(areaDiff)$
  # Reduce to get the mean squared difference. \
  reduceColumns(ee$Reducer$mean(), list("diff"))$
  get("mean")
)$sqrt()

# Print the result.
cat("RMSE=", rmse$getInfo())
