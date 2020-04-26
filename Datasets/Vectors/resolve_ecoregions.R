library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

set_color <- function(feature) {
  param <- ee$String(feature$get("COLOR"))$slice(1)
  feature %>%
    ee$Feature$set("R", ee$Number$parse(param$slice(0, 2), 16)) %>%
    ee$Feature$set("G", ee$Number$parse(param$slice(2, 4), 16)) %>%
    ee$Feature$set("B", ee$Number$parse(param$slice(4, 6), 16))
}

fc <- ee$FeatureCollection("RESOLVE/ECOREGIONS/2017")$
  map(function(f) set_color(f))

base <- ee$Image(0)$mask(0)$toInt8()

map_base <- base %>%
  ee$Image$paint(fc, "R") %>%
  ee$Image$addBands(base$paint(fc, "G")$addBands(base$paint(fc, "B")))

Map$setCenter(-98.87286, 38.84464, 3)
Map$addLayer(map_base, list(gamma = 0.3))
