#' Convert Earth Engine (EE) Table in a Simple Feature (sf)
#'
#' @param x ee.Geometry, ee.Feature or ee.FeatureCollection object to be converted into sf.
#' @importFrom geojsonio geojson_sf
#' @export
#' @details
#' The type of sf object returned will depend on the input GeoJSON. Sometimes you will get
#' back a POINTS class, and sometimes a POLYGON class, etc., depending on what the structure of the GeoJSON.
#' The reading and writing of the CRS to/from geojson is inconsistent. You can directly set the CRS by passing
#' a valid PROJ4 string or epsg code to the crs argument in st_read.
#' @return An sf class object, see Details.
#' @examples
#' library(ee)
#' ee_Initialize()
#' roi <- ee$Geometry$Polygon(list(
#'   c(-122.27577209472656, 37.891247253777074),
#'   c(-122.27577209472656, 37.86875557241152),
#'   c(-122.24040985107422, 37.86875557241152),
#'   c(-122.24040985107422, 37.891247253777074)
#' ))
#'
#' blocks <- ee$FeatureCollection("TIGER/2010/Blocks")
#' subset <- blocks$filterBounds(roi)
#' sf_subset <- ee_as_sf(subset)
#' plot(sf_subset)
ee_as_sf <- function(x) {
  x_local <- ee$FeatureCollection(x)$getInfo()
  class(x_local) <- "geo_list"
  x_local <- geojson_sf(x_local)
  return(x_local)
}
