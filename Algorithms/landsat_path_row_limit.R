library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

point <- ee$Geometry$Point(-98.7011, 47.2624)

collection <- ee$ImageCollection("LANDSAT/LC8_L1T_TOA")$
  filterBounds(point)$
  filterDate("2016-01-01", "2018-12-31")
new_col <- ee$Algorithms$Landsat$pathRowLimit(collection, 15, 100)
median <- new_col$median()

vis <- list(bands = c("B5", "B4", "B3"), max = 0.3)
Map$setCenter(-98.7011, 47.2624, 10)
Map$addLayer(median, vis, "Median")
