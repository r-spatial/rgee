library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

cal_area <- function(feature) {
  num <- ee$Number$parse(feature$get("areasqkm"))
  feature$set("areasqkm", num)
}

# Load watersheds from a data table.
sheds <- ee$FeatureCollection("USGS/WBD/2017/HUC06")$
  filterBounds(ee$Geometry$Rectangle(-127.18, 19.39, -62.75, 51.29))$
  map(cal_area)

# Display the table and print its first element.
# Map.addLayer(sheds, {}, 'watersheds')
Map$setCenter(-93.3, 33.88, 6)
Map$addLayer(
  eeObject = sheds,
  visParams = {},
  name = "watersheds"
)
print("First watershed")
ee_print(ee$Feature(sheds$first()))

# Print the number of watersheds.
cat("Count:", sheds$size()$getInfo())

# Print stats for an area property.
# print('Area stats:', sheds.aggregate_stats('areasqkm').getInfo())
