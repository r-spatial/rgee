#' Save an EE ImageCollection in their local system
#'
#' @param ic ee$ImageCollection to be saved in the system.
#' @param region EE Geometry Rectangle (ee$Geometry$Rectangle). The
#' CRS needs to be the same that the ic argument otherwise it will be
#' forced.
#' @param dsn Character. Output filename. If missing,
#' \code{ee_imagecollection_to_local} will create a temporary file.
#' @param scale Numeric. The resolution in meters per pixel. Defaults
#' to the native resolution of the image assset.
#' @param maxPixels Numeric. The maximum allowed number of pixels in the
#' exported image. The task will fail if the exported region covers
#' more pixels in the specified projection. Defaults to 100,000,000.
#' @param via Character. Method to fetch data about the object. Multiple
#' options supported. See details.
#' @param container Character. Name of the folder ('drive') or bucket ('gcs')
#' to be exported into (ignored if \code{via} is not defined as "drive" or
#' "gcs").
#' @param quiet logical. Suppress info message
#' @param ... Extra exporting argument. See \link{ee_image_to_drive} and
#' \link{ee_image_to_gcs}.
#' @details
#' \code{ee_imagecollection_to_local} supports the download of \code{ee$Image}
#' by three different options: "getInfo", "drive", and "gcs". When "getInfo"
#' is set in the \code{via} argument, \code{ee_imagecollection_to_local} will
#' make an REST call to retrieve all the known information about the object.
#' The advantage of use "getInfo" is a direct and faster download. However,
#' there is a limitation of 262144 pixels by request which makes it not
#' recommendable for large images. Instead of "getInfo", the options: "drive"
#' and "gcs" are suitable for large collections since they use an intermediate
#' container. They use Google Drive and Google Cloud Storage respectively. For
#' getting more information about exporting data from Earth Engine,  take a
#' look at the \href{https://developers.google.com/earth-engine/exporting}{Google
#' Earth Engine Guide - Export data}.
#' @importFrom crayon green
#' @return Character vector containing the filename of the images downloaded.
#' @family image download functions
#' @examples
#' \dontrun{
#' library(rgee)
#' library(raster)
#' ee_Initialize(drive = TRUE, gcs = TRUE)
#'
#' # USDA example
#' loc <- ee$Geometry$Point(-99.2222, 46.7816)
#' collection <- ee$ImageCollection('USDA/NAIP/DOQQ')$
#'   filterBounds(loc)$
#'   filterDate('2008-01-01', '2020-01-01')$
#'   filter(ee$Filter$listContains("system:band_names", "N"))
#'
#' # From ImageCollection to local directory
#' ee_crs <- collection$first()$projection()$getInfo()$crs
#' geometry <- collection$first()$geometry(proj = ee_crs)$bounds()
#' tmp <- tempdir()
#'
#' ## Using drive
#' ic_drive_files <- ee_imagecollection_to_local(
#'   ic = collection,
#'   region = geometry,
#'   scale = 100,
#'   dsn = file.path(tmp, "drive_"),
#'   via = "drive"
#' )
#'
#' }
#' @export
ee_imagecollection_to_local <- function(ic,
                                        region,
                                        dsn = NULL,
                                        via = "getInfo",
                                        scale = NULL,
                                        maxPixels = 1e9,
                                        container = "rgee_backup",
                                        quiet = FALSE,
                                        ...) {
  # is image an ee.image.Image?
  if (!any(class(ic) %in% "ee.imagecollection.ImageCollection")) {
    stop("ic argument is not an ee$imagecollection$ImageCollection")
  }

  # is region an ee.geometry.Geometry?
  if (!any(class(region) %in% "ee.geometry.Geometry")) {
    stop("region argument is not an ee$geometry$Geometry")
  }

  ic_names <- NULL
  ic_count <- ic$size()$getInfo()

  # if dsn is null
  if (is.null(dsn)) {
    ic_names <- ic$aggregate_array('system:index')$getInfo()
    if (is.null(ic_names)) {
      stop(
        "Error: ee_imagecollection_to_local was not able to create the ",
        "filenames of the images (dsn). Please Defined manually before ",
        "continuing."
      )
    }
  }

  # if dsn  is a vector character with the same length of the
  # imagecollection.
  if (length(dsn) == ic_count) {
    ic_names <- dsn
  } else {
    # if dsn is a directory or a character
    if (tryCatch(dir.exists(dsn), error = function(e) FALSE)) {
      ic_names <- ic$aggregate_array('system:index')$getInfo()
      ic_names <- sprintf("%s/%s",dsn,ic_names)
    }

    # if dsn is a directory or a character
    if (tryCatch(dir.exists(dirname(dsn)), error = function(e) FALSE)) {
      ic_names <- ic$aggregate_array('system:index')$getInfo()
      ic_names <- sprintf("%s%s",dsn,ic_names)
    }
  }

  # Output filename
  ic_names <- paste0(gsub("\\.tif$","", ic_names),".tif")

  if (!quiet) {
    cat(
      rule(
        right = bold(sprintf("%s - via %s", "Downloading ImageCollection", via))
      )
    )
    ee_geometry_message(region = region, quiet = quiet)
  }

  ic_files <- list()
  for (r_index in seq_len(ic_count)) {
    index <- r_index - 1
    image <- ee$Image(ic$toList(count = index + 1, offset = index)$get(0))
    if (!quiet) {
      cat(blue$bold("\nDownloading:"), green(ic_names[r_index]))
    }

    ee_image_local(
      image = image,
      region = region,
      dsn = ic_names[r_index],
      via = via,
      scale = scale,
      maxPixels = maxPixels,
      container = container,
      quiet = TRUE,
      ...
    )
    ic_files[[r_index]] <- ic_names[r_index]
  }
  if (!quiet) {
    cat("\n", rule())
  }
  as.character(ic_files)
}

#' geometry message
#' @importFrom crayon bold
#' @noRd
ee_geometry_message <- function(region, sf_region = NULL, quiet = FALSE) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("package sf required, please install it first")
  }
  # From geometry to sf
  if (is.null(sf_region)) {
    sf_region <- ee_as_sf(x = region)[["geometry"]]
  }

  sfg_geom_data <- sf::st_as_text(sf_region)
  current_lenght <- nchar(sfg_geom_data)
  if (current_lenght > 60) {
    sfg_geom_data <- paste0(
      substr(sfg_geom_data,1, 27),
      " .... ",
      substr(sfg_geom_data, current_lenght - 27, current_lenght)
    )
  }

  region_crs <- sf::st_crs(sf_region)[["wkt"]]
  region_crs_summary <- strsplit(region_crs, "\n")[[1]][1:3] %>%
    paste0(collapse = "\n") %>%
    paste0(bold(" ....."))

  ### Metadata ----
  #is geodesic?
  is_geodesic <- region$geodesic()$getInfo()
  #is evenodd?
  query_params <- unlist(jsonlite::parse_json(region$serialize())$scope)
  is_evenodd <- all(as.logical(
    query_params[grepl("evenOdd", names(query_params))]
  ))
  if (length(is_evenodd) == 0 | is.null(is_evenodd)) {
    is_evenodd <- TRUE
  }
  ### ------------
  # geom message to display
  if (!quiet) {
    cat(
      bold("- region parameters\n"),
      bold("sfg      :"), sfg_geom_data, "\n",
      bold("CRS      :"), region_crs_summary, "\n",
      bold("geodesic :"), ee_utils_py_to_r(is_geodesic), "\n",
      bold("evenOdd  :"), is_evenodd, "\n"
    )
  }
  invisible(TRUE)
}
