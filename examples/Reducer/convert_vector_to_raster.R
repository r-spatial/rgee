library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Load a collection of US counties.
counties <- ee$FeatureCollection("TIGER/2018/Counties")

# Make an image out of the land area attribute.
landAreaImg <- counties$
  filter(ee$Filter$notNull(list("ALAND")))$
  reduceToImage(
  properties = list("ALAND"),
  reducer = ee$Reducer$first()
)

# Display the county land area image.
Map$setCenter(-99.976, 40.38, 5)
Map$addLayer(
  eeObject = landAreaImg,
  visParams = list(min = 3e8, max = 1.5e10, palette = c("FCFDBF", "FDAE78", "EE605E", "B63679", "711F81", "2C105C")),
  name = "Land Area"
)
