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
#' getting more information about exporting data take a look at the
#' \href{https://developers.google.com/earth-engine/exporting}{Google Earth
#' Engine Guide - Export data}.
#' @return A character object
ee_imagecollection_to_local <- function(ic,
                                        region,
                                        dsn = NULL,
                                        via = "getInfo",
                                        scale = NULL,
                                        maxPixels = 1e9,
                                        container = "rgee_backup",
                                        quiet = FALSE) {
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

  # if dsn is a directory or a character
  if (tryCatch(dir.exists(dsn), error = function(e) FALSE)) {
    ic_names <- ic$aggregate_array('system:index')$getInfo()
    ic_names <- sprintf("%s/%s",dsn,ic_names)
  }

  # if dsn is a directory or a character
  if (tryCatch(dir.exists(dirname(dsn)), error = function(e) FALSE)) {
    ic_names <- ic$aggregate_array('system:index')$getInfo()
    ic_names <- sprintf("%s_%s",dsn,ic_names)
  }

  # if dsn  is a vector character with the same length of the
  # imagecollection.
  if (length(dsn) == ic_count) {
    ic_names <- dsn
  }

  ic_names <- paste0(gsub("\\.tif$","", ic_names),".tif")

  ic_files <- list()
  for (r_index in seq_len(ic_count)) {
    index <- r_index - 1
    image <- ee$Image(ic$toList(count = index + 1, offset = index)$get(0))
    img_files <- ee_image_to_local(
      image = image,
      region = region,
      dsn = ic_names[r_index],
      via = via,
      scale = scale,
      maxPixels = maxPixels,
      container = container,
      quiet = quiet
    )
    ic_files[[r_index]] <- ic_names[r_index]
  }
  as.character(ic_files)
}

