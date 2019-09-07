#' Convert an EE table in a sf object
#'
#' @param x EE table to be converted into a sf object.
#' @importFrom geojsonio geojson_sf
#' @details
#' For exporting large spatial objects is better creates export pipelines through `ee$batch$Export$table$*`
#' and `rgee::ee_download_*` instead of using `ee_as_sf`. See
#' \href{https://developers.google.com/earth-engine/client_server#client-and-server-functions}{Client vs Server}
#' documentation for details.
#' @return An sf class object, see Details.
#' @examples
#' library(rgee)
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
#' cat('Object size in Mb:', as.numeric(object.size(sf_subset)/10^6))
#' plot(sf_subset)
#' # Define an arbitrary region in which to compute random points.
#' region <- ee$Geometry$Rectangle(-119.224, 34.669, -99.536, 50.064)
#' # Create 1000 random points in the region.
#' ee_randomPoints <- ee$FeatureCollection$randomPoints(region)
#' sf_randomPoints <- ee_as_sf(ee_randomPoints)
#' plot(sf_randomPoints)
#' @export
ee_as_sf <- function(x) {
  if (!"ee.computedobject.ComputedObject" %in% class(x)) {
    stop("x is not a Earth Engine object")
  }
  x_local <- ee$FeatureCollection(x)$getInfo()
  class(x_local) <- "geo_list"
  x_local <- geojson_sf(x_local)
  return(x_local)
}
