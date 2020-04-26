library(rgee)
library(sf)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

sf_shp <- st_read(system.file("shape/nc.shp", package = "sf")) %>%
  sf_as_ee()

Map$addLayer(sf_shp)
buffered <- sf_shp$map(function(x) x$buffer(2000))
unioned <- buffered$union()

Map$addLayer(
  eeObject = unioned,
  visParams = list(color = "800080"),
  name = "BART stations"
)
