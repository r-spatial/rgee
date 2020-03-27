#' HDR Landsat.
#'
#' Display portions of an image with different
#' dynamic ranges. The land areas are displayed
#' normally, but the water areas are streched to
#' show more details.

library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Filter the LE7 collection to a single date.
collection <- (ee$ImageCollection("LE7_L1T")
$filterDate(c("2002-11-08"), "2002-11-09"))
image <- collection$mosaic()$select("B3", "B2", "B1")

# Display the image normally.
Map$setCenter(-95.738, 18.453, 9)
Map$addLayer(image, list(gain = "1.6, 1.4, 1.1"), "Land")

# Add and stretch the water.  Once where the elevation is masked,
# and again where the elevation is zero.
elev <- ee$Image("srtm90_v4")
mask1 <- elev$mask()$eq(0)$And(image$mask())
mask2 <- elev$eq(0)$And(image$mask())

Map$addLayer(
  eeObject = image$mask(mask1),
  visParams = list(gain = 6.0, bias = -200),
  name = "Water: Masked"
) +
Map$addLayer(
  eeObject = image$mask(mask2),
  visParams = list(gain = 6.0, bias = -200),
  name = "Water: Elev 0"
)
