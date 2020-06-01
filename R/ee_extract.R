#' Extract values from EE Images or ImageCollections objects
#'
#' Extract values from a \code{ee$Image} or \code{ImageCollection} at the
#' locations of a geometry object. You can use \code{ee$Geometry$*},
#' \code{ee$Feature}, \code{ee$FeatureCollection} and sf objects. This function
#' mimicking how \link[raster]{extract} currently works.
#'
#' @param x ee$Image or ee$ImageCollection with a single band.
#' @param y ee$Geometry$*, ee$Feature, ee$FeatureCollection or sf objects.
#' @param fun ee$Reducer object. Function to summarize the values. The function
#' must take a single numeric value as an argument and return a single value.
#' See details.
#' @param scale A nominal scale in meters of the Image projection to work in.
#' By default 1000.
#' @param sf Logical. Should the extracted values be added to the data.frame of
#' the sf object y?
#' @param ... reduceRegions additional parameters. See
#' \code{ee_help(ee$Image$reduceRegions)} for more details.
#'
#' @importFrom sf st_geometry st_geometry<- st_drop_geometry
#'
#' @return A data.frame or an sf object depending on the sf argument. The
#' columns receive their name from the image
#' metadata property \code{RGEE_NAME}. If it is not defined \code{ee_extract}
#' use the band name (\code{ee$Image$name}) if \code{x} is an \code{ee$Image}
#' and the \code{system:index} property if \code{x} is an
#' \code{ee$ImageCollection}.
#'
#' @details
#' The reducer functions that return one value are:
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
#' \dontrun{
#' library(rgee)
#' library(sf)
#'
#' ee_reattach() # reattach ee as a reserved word
#' ee_Initialize()
#'
#' # Define a Image or ImageCollection: Terraclimate
#' terraclimate <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
#'   filterDate("2001-01-01", "2002-01-01")$
#'   map(function(x){
#'     date <- ee$Date(x$get("system:time_start"))$format('YYYY_MM_dd')
#'     name <- ee$String$cat("Terraclimate_pp_", date)
#'     x$select("pr")$reproject("EPSG:4326")$set("RGEE_NAME", name)
#'   })
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
#'   fun = ee$Reducer$mean(),
#'   sf = TRUE
#' )
#'
#' # Spatial plot
#' plot(
#'   ee_nc_rain["Terraclimate_pp_2001_01_01"],
#'   main = "2001 Jan Precipitation - Terraclimate",
#'   reset = FALSE
#' )
#' }
#' @export
ee_extract <- function(x,
                       y,
                       fun = ee$Reducer$mean(),
                       scale = 1000,
                       sf = FALSE,
                       ...) {
  # spatial classes
  sf_classes <- c("sf", "sfc", "sfg")
  sp_objects <- ee_get_spatial_objects('Table')
  x_type <- x$name()

  # datatype
  # Load Python module
  oauth_func_path <- system.file("python/ee_extract.py", package = "rgee")
  extract_py <- ee_source_python(oauth_func_path)

  # Is y a Spatial object?
  if (!any(class(y) %in% c(sp_objects, sf_classes))) {
    stop("y is not a Earth Engine table or a sf object.")
  }

  # Is x a Image or ImageCollection?
  if (!any(class(x) %in% ee_get_spatial_objects("i+ic"))) {
    stop("x is neither an ee$Image nor ee$ImageCollection")
  }

  # Is a complex ImageCollection?
  if (x_type == "ImageCollection") {
    band_names <- x$first()$bandNames()$getInfo()
    if (length(band_names) > 1) {
      stop(
        "ee_extract does not support ee$ImageCollection with",
        " multiple bands"," \nEntered: ",
        paste0(band_names,collapse = " "),
        "\nExpected: ",
        band_names[1]
      )
    }
  } else {
    band_names <- x$bandNames()$getInfo()
    img_to_ic <- function(index) x$select(ee$String(x$bandNames()$get(index)))
    # Force to x to be a ImageCollection
    x <- ee$ImageCollection$fromImages(
      lapply(seq_along(band_names) - 1 , img_to_ic)
    )
  }

  # RGEE_NAME exist?
  if (is.null(x$first()$get("RGEE_NAME")$getInfo())) {
    if (x_type == "ImageCollection") {
      x <- x$map(function(img) img$set("RGEE_NAME", img$get("system:index")))
    } else {
      x <- x$map(function(img) img$set("RGEE_NAME", ee$String(img$bandNames())))
    }
  }

  # If y is a sf object convert into a ee$FeatureCollection object
  if (any(sf_classes %in% class(y))) {
    sf_y <- y
    ee_y <- sf_as_ee(y)
  }

  # If y is a ee$FeatureCollection object and sf is TRUE convert it to an
  # sf object
  if (any(ee_get_spatial_objects('Table') %in%  class(y))) {
    ee_y <- ee$FeatureCollection(y)
    if (isTRUE(sf)) {
      sf_y <- ee_as_sf(y)
    }
  }

  #set ee_ID for identify rows in the data.frame
  ee_y <- ee_y$map(function(f) f$set("ee_ID", f$get("system:index")))

  # Get the funname
  fun_name <- gsub("Reducer.", "", fun$getInfo()["type"])

  # triplets save info about the value, the row_id (ee_ID) and
  # col_id (imageId)
  triplets <- x$map(function(image) {
    image$reduceRegions(
      collection = ee_y,
      reducer = fun,
      scale = scale,
      ...
    )$map(function(f) f$set("imageId", image$get("RGEE_NAME")))
  })$flatten()

  # From ee$Dict format to a table
  table <- extract_py$
    table_format(triplets, "ee_ID", "imageId", fun_name)$
    map(function(feature) {
    feature$setGeometry(NULL)
  })

  # Extracting data and passing to sf
  table_geojson <- ee_utils_py_to_r(table$getInfo())
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
