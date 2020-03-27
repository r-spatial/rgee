library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

image <- ee$Image("LANDSAT/LC8_L1T/LC80260412017023LGN00")
b5 <- image$select("B5")

Map$centerObject(image, zoom = 10)
Map$addLayer(image, name = "Band 5")

selected <- image$select(
  opt_selectors = c("B5", "B4", "B3"),
  opt_names = c("Nir", "Red", "Green")
)

Map$addLayer(selected, name = "Renamed bands")
print(selected$bandNames()$getInfo())
