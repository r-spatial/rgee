library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Make a date filter to get images in this date range.
dateFilter <- ee$Filter$date("2014-01-01", "2014-02-01")

# Load a MODIS collection with EVI data.
mcd43a4 <- ee$ImageCollection("MODIS/MCD43A4_006_EVI")$
  filter(dateFilter)

# Load a MODIS collection with quality data.
mcd43a2 <- ee$ImageCollection("MODIS/006/MCD43A2")$
  filter(dateFilter)

# Define an inner join.
innerJoin <- ee$Join$inner()

# Specify an equals filter for image timestamps.
filterTimeEq <- ee$Filter$equals(
  leftField = "system:time_start",
  rightField = "system:time_start"
)

# Apply the join.
innerJoinedMODIS <- innerJoin$apply(mcd43a4, mcd43a2, filterTimeEq)

# Display the join result: a FeatureCollection.
ee_print(innerJoinedMODIS)

# Map a function to merge the results in the output FeatureCollection.
get_prop <- function(feature) {
  ee$Image$cat(feature$get("primary"), feature$get("secondary"))
}

joinedMODIS <- innerJoinedMODIS$map(get_prop)

# Print the result of merging.
print("Inner join, merged bands:")
ee_print(joinedMODIS)
