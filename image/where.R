#' Where operator example.
#' Select the forest classes from the MODIS land cover
#' image and intersect them with elevations above 1000m.

library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

elev <- ee$Image("srtm90_v4")
cover <- ee$Image("MCD12Q1/MCD12Q1_005_2001_01_01")$select("Land_Cover_Type_1")
blank <- ee$Image(0)

# Where (1 <= cover <= 4) and (elev > 1000), set the output to 1.
output <- blank$where(
  test = cover$lte(4)$And(cover$gte(1))$And(elev$gt(1000)),
  value = 1
)

# Output contains 0s and 1s.  Mask it with itself to get rid of the 0s.
result <- output$mask(output)
vis <- list(min = 0, max = 3000)
Map$setCenter(-113.41842, 40.055489, 6)
Map$addLayer(elev, vis, "SRTM") +
Map$addLayer(result, list(palette = "00AA00"), "Land Cover")
