#' Map an expression.
#'
#' Computes the mean NDVI and SAVI by mapping an expression over a collection
#' and taking the mean.  This intentionally exercises both variants of
#' Image$expression.

library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()


fc <- ee$FeatureCollection("TIGER/2018/States")$filter(ee$Filter$eq("STUSPS", "MN"))

# Filter the L7 collection to a single month.
collection <- (ee$ImageCollection("LE7_L1T_TOA")
$filterDate("2002-11-01", "2002-12-01")
$filterBounds(fc))

# A function to compute NDVI.
NDVI <- function(image) {
  image$expression('float(b("B4") - b("B3")) / (b("B4") + b("B3"))')
}

# A function to compute Soil Adjusted Vegetation Index.
SAVI <- function(image) {
  ee$Image(0)$expression(
    expression = "(1 + L) * float(nir - red)/ (nir + red + L)",
    opt_map = list(
      nir = image$select("B4"),
      red = image$select("B3"),
      L = 0.2
    )
  )
}

vis <- list(
  min = 0,
  max = 1,
  palette = c(
    "FFFFFF", "CE7E45", "DF923D", "F1B555", "FCD163",
    "99B718", "74A901", "66A000", "529400", "3E8601",
    "207401", "056201", "004C00", "023B01", "012E01",
    "011D01", "011301"
  )
)

Map$setCenter(-94.12286, 45.83495, 5)
Map$addLayer(collection$map(NDVI)$mean(), vis) +
Map$addLayer(collection$map(SAVI)$mean(), vis)
