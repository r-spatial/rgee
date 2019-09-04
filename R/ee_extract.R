#' Extract values from Image EE spatial objects
#' Extract values from a Image or ImageCollection spatial object at the locations
#' of geometry object. You can use coordinates, ee.Geometries, ee.Features,
#' ee.FeatureCollection and sf objects.
#' @param x ee$Image or ee$ImageCollection.
#' @param y ee$Geometry, ee$Feature, ee$FeatureCollection or sf object.
#' @param fun ee$Reducer object. Function to summarize the values. See details.
#' @param ... reduceRegions parameters. See Details.
#' library(rgee)
#' ee_Initialize()
#' terraclimate = ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
#'   filterDate("2000-01-01","2001-01-01")$
#'   map(function(x) x$select("pr"))
#'
#' ee_nc = st_read(system.file("shape/nc.shp", package="sf")) %>%
#'   st_transform(4326) %>%
#'   sf_as_ee()
#' ee_nc_rain = ee_extract(terraclimate,ee_nc)
#' ee_nc_rain
ee_extract <- function(x, y, fun = ee$Reducer$mean(), scale = 1000, sf = TRUE, ...) {
  y = ee$FeatureCollection(y)$map(function(x) x$set('ID',x$get("system:index")))
  fun_name = gsub("Reducer.","",fun$getInfo()['type'])
  triplets = ee$ImageCollection(x)$map(function(image) {
    image$reduceRegions(collection = y,
                        reducer = fun,
                        scale = scale)$
      map(function(f) f$set("imageId", image$id()))
  })$flatten()
  table = py$table_format(triplets, 'ID', 'imageId',fun_name)
  #table = table_format(triplets, 'FIPS', 'imageId')
  table_geojson = table$getInfo()
  class(table_geojson) = "geo_list"
  table_sf = geojson_sf(table_geojson)
  return(table_sf)
}
