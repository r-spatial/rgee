#' Computed area filter example.
#' Find US counties smaller than 3k square kilometers in area.
library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

Map$setCenter(-119.7, 38.26, 7)

counties <- ee$FeatureCollection("TIGER/2016/Counties")
counties_with_area <- counties$map(function(f) f$set(list(area = f$area())))
small_counties <- counties_with_area$filterMetadata("area", "less_than", 3e9)

Map$addLayer(small_counties, list(color = "900000"), "Small US counties")
