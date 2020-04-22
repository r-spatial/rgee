library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()


startTime <- "2001-01-01"
endTime <- "2001-02-01"

lst <- ee$ImageCollection("FORA0125_H002")$
  filterDate(startTime, endTime)

# Get the time series at these points.
points <- ee$Geometry$Point(-85.16516, 30.85000)
collection <- ee$FeatureCollection(points)


# Extract the values by running reduceRegions over each image in the image collection.
fc_reduceRegions <- function(feature) {
  feature$reduceRegions(collection, "first")
}

values <- lst$map(fc_reduceRegions)$flatten()

# Turn the result into a feature collection and export it.
taskParams <- list(
  driveFolder = "image",
  driveFileNamePrefix = "TylerTest",
  fileFormat = "CSV"
)

MyTry <- ee$batch$Export$table(values, "lst_timeseries", taskParams)
MyTry$start()
ee_monitoring(MyTry)
