library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()


#' Reverse mask a region. Create an image that masks
#' everything except for the specified polygon.
#'
fc = ee$FeatureCollection('RESOLVE/ECOREGIONS/2017')$
  filter(ee$Filter()$eq('ECO_NAME', 'Great Basin shrub steppe'))

# Start with a black image.
empty = ee$Image(0)$toByte()
# Fill and outline the polygons in two colors
filled = empty$paint(fc, 2)
both = filled$paint(fc, 1, 5)
# Mask off everything that matches the fill color.
result = both$mask(filled$neq(2))

Map$setCenter(-115, 40, 4)
Map$addLayer(
  eeObject = result,
  visParams = list(
    palette = c('000000,FF0000'),
    max = 1,
    opacity = 0.5
  ),
  name =  "Basin"
)
