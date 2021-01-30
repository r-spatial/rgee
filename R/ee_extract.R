#' Extract values from EE Images or ImageCollections objects
#'
#' Extract values from a \code{ee$Image} at the
#' locations of a geometry object. You can use \code{ee$Geometry$*},
#' \code{ee$Feature}, \code{ee$FeatureCollection}, sf  or sfc objects. This function
#' mimicking how \link[raster]{extract} currently works.
#'
#' @param x ee$Image.
#' @param y ee$Geometry$*, ee$Feature, ee$FeatureCollection, sfc or sf objects.
#' @param fun ee$Reducer object. Function to summarize the values. The function
#' must take a single numeric value as an argument and return a single value.
#' See details.
#' @param scale A nominal scale in meters of the Image projection to work in.
#' By default 1000.
#' @param sf Logical. Should return a sf object?
#' @param quiet Logical. Suppress info message.
#' @param ... ee$Image$reduceRegions additional parameters. See
#' \code{ee_help(ee$Image$reduceRegions)} for more details.
#'
#' @return A data.frame or an sf object depending on the sf argument.
#' Column names are extracted from band names, use \code{ee$Image$rename} to
#' rename the bands of an \code{ee$Image}. See \code{ee_help(ee$Image$rename)}.
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
#' ee_Initialize()
#'
#' # Define a Image or ImageCollection: Terraclimate
#' terraclimate <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE") %>%
#'   ee$ImageCollection$filterDate("2001-01-01", "2002-01-01") %>%
#'   ee$ImageCollection$map(
#'     function(x) {
#'       date <- ee$Date(x$get("system:time_start"))$format('YYYY_MM_dd')
#'       name <- ee$String$cat("Terraclimate_pp_", date)
#'       x$select("pr")$rename(name)
#'     }
#'   )
#'
#' # Define a geometry
#' nc <- st_read(
#'   dsn = system.file("shape/nc.shp", package = "sf"),
#'   stringsAsFactors = FALSE,
#'   quiet = TRUE
#' )
#'
#'
#' # Extract values
#'
#' ee_nc_rain <- ee_extract(
#'   x = terraclimate,
#'   y = nc["NAME"],
#'   scale = 250,
#'   fun = ee$Reducer$mean(),
#'   sf = TRUE
#' )
#'
#' # Spatial plot
#' plot(
#'   ee_nc_rain["X200101_Terraclimate_pp_2001_01_01"],
#'   main = "2001 Jan Precipitation - Terraclimate",
#'   reset = FALSE
#' )
#' }
#' @export
ee_extract <- function(x,
                       y,
                       fun = ee$Reducer$mean(),
                       scale = NULL,
                       sf = FALSE,
                       quiet = FALSE,
                       ...) {
  ee_check_packages("ee_extract", c("geojsonio", "sf"))

  # print scale
  if (!quiet & is.null(scale)) {
    scale <- 1000
    message(sprintf("The image scale is set to %s.", scale))
  }

  # Is x a Image or ImageCollection?
  if (!any(class(x) %in% ee_get_spatial_objects("i+ic"))) {
    stop("x is neither an ee$Image nor ee$ImageCollection")
  }

  # Is x a ImageCollection?
  if (any(class(x) %in%  "ee.imagecollection.ImageCollection")) {
    # if (!quiet) {
    #   message("x is an ImageCollection, running 'x$toBands()' to ",
    #           "convert it into an Image")
    # }
    x <- ee$ImageCollection$toBands(x)
  }

  # Load Python module
  oauth_func_path <- system.file("python/ee_extract.py", package = "rgee")
  extract_py <- ee_source_python(oauth_func_path)
  # spatial classes
  sp_objects <- ee_get_spatial_objects('Table')

  # Is y a Spatial object?
  if (!any(class(y) %in% c("sf", "sfc", sp_objects))) {
    stop("y is not a sf, sfc, ee$Geometry, ee$Feature or ee$FeatureCollection object.")
  }

  # If y is a sf object convert into a ee$FeatureCollection object
  if (any("sf" %in% class(y))) {
    sf_y <- y
    # if (!quiet) {
    #   message("NOTE: y is an sf object, running 'sf_as_ee(y$geometry)' to ",
    #           "convert in an ee$FeatureCollection object.")
    # }
    ee_y <- sf_as_ee(y[["geometry"]], quiet = TRUE)
  } else if(any("sfc" %in%  class(y))) {
    sf_y <- sf::st_sf(id = seq_along(y), geometry = y)
    # if (!quiet) {
    #   message("y is an sfc object, running 'sf_as_ee(y)' to ",
    #           "convert it into an ee$FeatureCollection object.")
    # }
    ee_y <- sf_as_ee(y, quiet = TRUE)
    # If y is a ee$FeatureCollection object and 'sf' arg is TRUE convert it to an
    # sf object
  } else if(any(ee_get_spatial_objects('Table') %in%  class(y))) {
    ee_y <- ee$FeatureCollection(y)
    sf_y <- ee_as_sf(y, quiet = TRUE)
  }

  #set ee_ID for identify rows in the data.frame
  ee_add_rows <- function(f) {
    f_prop <- ee$Feature$get(f, "system:index")
    ee$Feature(ee$Feature$set(f, "ee_ID", f_prop))
  }
  ee_y <- ee$FeatureCollection(ee_y) %>%
    ee$FeatureCollection$map(ee_add_rows)

  # Get the funname
  fun_name <- gsub("Reducer.", "", (ee$Reducer$getInfo(fun))[["type"]])

  # Convert Image into ImageCollection
  x_ic <- bands_to_image_collection(x)


  # triplets save info about the value, the row_id (ee_ID) and col_id (imageId)
  create_tripplets <- function(img) {
    img_reduce_regions <- ee$Image$reduceRegions(
      image = img,
      collection = ee_y,
      reducer = fun,
      scale = scale,
      ...
    )
    ee$FeatureCollection$map(
      img_reduce_regions,
      function(f) {
        ee$Feature$set(f, "imageId", ee$Image$get(img, "system:index"))
      }
    )
  }

  triplets <- x_ic %>%
    ee$ImageCollection$map(create_tripplets) %>%
    ee$ImageCollection$flatten()

  # From ee$Dict format to a table
  table <- extract_py$
    table_format(triplets, "ee_ID", "imageId", fun_name)$
    map(function(feature) {
      ee$Feature$setGeometry(feature, NULL)
    })

  # Extracting data and passing to sf
  table_geojson <- table %>%
    ee$FeatureCollection$getInfo() %>%
    ee_utils_py_to_r()
  class(table_geojson) <- "geo_list"
  table_sf <- geojsonio::geojson_sf(table_geojson)
  sf::st_geometry(table_sf) <- NULL
  table_sf <- table_sf[, order(names(table_sf))]

  # Removing helper index's
  table_sf["id"] <- NULL
  table_sf["ee_ID"] <- NULL

  # Remove system:index prefix
  #colnames(table_sf) <- gsub("^[^_]*_","", colnames(table_sf))

  if (isTRUE(sf)) {
    table_geometry  <- sf::st_geometry(sf_y)
    table_sf <- sf_y %>%
      sf::st_drop_geometry() %>%
      cbind(table_sf) %>%
      sf::st_sf(geometry = table_geometry)
  } else {
    table_sf <- sf_y %>%
      sf::st_drop_geometry() %>%
      cbind(table_sf)
  }
  table_sf
}

#' Converts all bands in an image to an image collection.
#' @param img ee$Image. The image to convert.
#' @return ee$ImageCollection
#' @noRd
bands_to_image_collection <- function(img) {
  bname_to_image <- function(band) {
    img %>%
      ee$Image$select(ee$String(band)) %>%
      ee$Image$set(
        list("system:index" = ee$String(band))
      )
  }
  img %>%
    ee$Image$bandNames() %>%
    ee$List$map(ee_utils_pyfunc(bname_to_image)) %>%
    ee$ImageCollection()
}
