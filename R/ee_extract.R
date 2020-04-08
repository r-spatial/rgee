#' Extract values from EE Images or ImageCollections objects
#'
#' Extract values from a \code{ee$Image} or \code{ImageCollection} at the
#' locations of a geometry object. You can use \code{ee$Geometry$*},
#' \code{ee$Feature}, \code{ee$FeatureCollection} and sf objects. This function
#' try to mimics how \link[raster]{extract} currently works.
#'
#' @param x EE Image or ImageCollection.
#' @param y ee$Geometry$*, ee$Feature, ee$FeatureCollection or sf objects.
#' @param fun ee$Reducer object. Function to summarize the values. The function
#' should take a single numeric vector as argument and return a single value.
#' See details.
#' @param scale A nominal scale in meters of the Image projection to work in.
#' By default 1000.
#' @param sf Logical. Should the extracted values be added to the data.frame of
#' the sf object y? This only applies if y is a sf object.
#' @param ... reduceRegions additional parameters. See
#' ee_help(ee$Image()$reduceRegions) for more details.
#' @importFrom sf st_geometry st_geometry<- st_drop_geometry
#' @details
#' In Google Earth Engine the reducer functions that return one value are:
#' \itemize{
#' \item  \strong{allNonZero}: Returns a Reducer that returns 1 if all of its
#' inputs are non-zero, 0 otherwise. \cr
#' \item \strong{anyNonZero}: Returns a Reducer that returns 1 if any of its
#' inputs are non-zero, 0 otherwise. \cr
#' \item \strong{bitwiseAnd}: Returns a Reducer that computes the bitwise-and
#' summation of its inputs.
#' \item \strong{bitwiseOr}: Returns a Reducer that computes the bitwise-or
#' summation of its inputs.
#' \item \strong{count}: Returns a Reducer that computes the number of
#' non-null inputs.
#' \item \strong{first}: Returns a Reducer that returns the first of its inputs.
#' \item \strong{firstNonNull}: Returns a Reducer that returns the first of
#' its non-null inputs.
#' \item \strong{kurtosis}: Returns a Reducer that Computes the kurtosis of
#' its inputs.
#' \item \strong{last}: Returns a Reducer that returns the last of its inputs.
#' \item \strong{lastNonNull}: Returns a Reducer that returns the last of its
#' non-null inputs.
#' \item \strong{max}: Creates a reducer that outputs the maximum value of
#' its (first) input. If numInputs is greater than one, also outputs the
#' corresponding values of the additional inputs.
#' \item \strong{mean}: Returns a Reducer that computes the (weighted)
#' arithmetic mean of its inputs.
#' \item \strong{median}: Create a reducer that will compute the median of
#' the inputs. For small numbers of inputs (up to maxRaw) the median will be
#' computed directly; for larger numbers of inputs the median will be derived
#' from a histogram.
#' \item \strong{min}: Creates a reducer that outputs the minimum value
#' of its (first) input.  If numInputs is greater than one, also outputs
#' additional inputs.
#' \item \strong{mode}: Create a reducer that will compute the mode of the
#' inputs.  For small numbers of inputs (up to maxRaw) the mode will be
#' computed directly; for larger numbers of inputs the mode will be derived
#' from a histogram.
#' \item \strong{product}: Returns a Reducer that computes the product of
#' its inputs.
#' \item \strong{sampleStdDev}: Returns a Reducer that computes the sample
#' standard deviation of its inputs.
#' \item \strong{sampleVariance}: Returns a Reducer that computes the sample
#' variance of its inputs.
#' \item \strong{stdDev}: Returns a Reducer that computes the standard
#' deviation of its inputs.
#' \item \strong{sum}: Returns a Reducer that computes the (weighted) sum
#' of its inputs.
#' \item \strong{variance}: Returns a Reducer that computes the variance
#' of its inputs.
#' }
#' @examples
#' library(rgee)
#' library(sf)
#'
#' ee_reattach() # reattach ee as a reserved word
#' ee_Initialize()
#'
#' # Define a Image or ImageCollection: Terraclimate
#' terraclimate <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
#'   filterDate("2001-01-01", "2002-01-01")$
#'   map(function(x) x$select("pr"))
#'
#' # Define a geometry
#' nc <- st_read(
#'   dsn = system.file("shape/nc.shp", package = "sf"),
#'   stringsAsFactors = FALSE,
#'   quiet = TRUE
#' )
#'
#' # Extract values
#' ee_nc_rain <- ee_extract(
#'   x = terraclimate,
#'   y = nc,
#'   scale = 250,
#'   fun = ee$Reducer$max(),
#'   sf = TRUE
#' )
#'
#' # Spatial plot
#' plot(ee_nc_rain["X200106"],
#'   main = "2001 Jan Precipitation - Terraclimate",
#'   reset = FALSE
#' )
#' dev.off()
#'
#' # Temporal plot
#' ee_nc <- ee_nc_rain
#' time_serie <- as.numeric(ee_nc[1, sprintf("X%s", 200101:200112)])
#' main <- sprintf("2001 Precipitation - %s", ee_nc$NAME[1])
#' plot(time_serie, ylab = "pp (mm/month)", type = "l", lwd = 1.5, main = main)
#' points(time_serie, pch = 20, lwd = 1.5, cex = 1.5)
#' @export
ee_extract <- function(x,
                       y,
                       fun = ee$Reducer$mean(),
                       scale = 1000,
                       sf = FALSE,
                       ...) {
  oauth_func_path <- system.file("python/ee_extract.py", package = "rgee")
  extract_py <- ee_source_python(oauth_func_path)
  if (any(c("sf", "sfc", "sfg") %in% class(y))) {
    sf_y <- y
    y <- sf_as_ee(y)
  }

  # Set the index image as a property in the FeatureCollection
  y <- ee$FeatureCollection(y)$map(
    function(x) x$set("ee_ID", x$get("system:index"))
  )
  fun_name <- gsub("Reducer.", "", fun$getInfo()["type"])

  # Handling the images
  triplets <- ee$ImageCollection(x)$map(function(image) {
    image$reduceRegions(
      collection = y,
      reducer = fun,
      scale = scale
    )$map(function(f) f$set("imageId", image$id()))
  })$flatten()
  table <- extract_py$
    table_format(triplets, "ee_ID", "imageId", fun_name)$
    map(function(feature) {
    feature$setGeometry(NULL)
  })

  # Extracting data and passing to sf
  table_geojson <- ee_py_to_r(table$getInfo())
  class(table_geojson) <- "geo_list"
  table_sf <- geojson_sf(table_geojson)
  st_geometry(table_sf) <- NULL
  table_sf <- table_sf[, order(names(table_sf))]

  # Removing helper index's
  table_sf["id"] <- NULL
  table_sf["ee_ID"] <- NULL

  if (isTRUE(sf)) {
    table_geometry  <- st_geometry(sf_y)
    table_sf <- sf_y %>%
      st_drop_geometry() %>%
      cbind(table_sf) %>%
      st_sf(geometry = table_geometry)
  }
  table_sf
}


#' Convert a character if it is a factor
#' @noRd
ee_isfactor_to_character <- function(x){
  if (is(x,"factor")) {
    as.character(x)
  } else {
    x
  }
}
