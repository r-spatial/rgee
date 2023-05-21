#' Save an EE ImageCollection to the local system.
#'
#' @param ic ee$ImageCollection to be saved to the system.
#' @param region EE Geometry (ee$Geometry$Polygon). The
#' CRS needs to be the same that the \code{ic} argument. Otherwise, it will be
#' forced.
#' @param dsn Character. Output filename. If missing, a temporary file will
#' be created for each image.
#' @param via Character. Method to export the image. Two methods are available:
#' "drive", "gcs". See details.
#' @param container Character. Name of the folder ('drive') or bucket ('gcs')
#' to be exported into (ignored if \code{via} is not defined as "drive" or
#' "gcs").
#' @param scale Numeric. The resolution in meters per pixel. Defaults
#' to the native resolution of the image.
#' @param maxPixels Numeric. The maximum allowable number of pixels in the exported image.
#' If the exported region covers more pixels than the specified limit in the given projection,
#' the task will fail. Defaults to 100,000,000.
#' @param lazy Logical. If TRUE, a \code{\link[future:sequential]{
#' future::sequential}} object is created to evaluate the task in the future.
#' See details.
#' @param public Logical. If TRUE, a public link to the image is created.
#' @param add_metadata Add metadata to the stars_proxy object. See details.
#' @param timePrefix Logical. Add current date and time (\code{Sys.time()}) as
#' a prefix to export files. This parameter helps to avoid exporting files
#' with the same name. By default TRUE.
#' @param quiet Logical. Suppress info message
#' @param ... Extra exporting argument. See \link{ee_image_to_drive} and
#' @details
#' \code{ee_imagecollection_to_local} supports the download of \code{ee$Images}
#' using two different options: "drive"
#' (\href{https://CRAN.R-project.org/package=googledrive}{Google Drive}) and "gcs"
#' (\href{https://CRAN.R-project.org/package=googleCloudStorageR}{
#' Google Cloud Storage}). In both cases, \code{ee_imagecollection_to_local}
#' works as follow:
#' \itemize{
#'   \item{1. }{A task  is initiate (i.e., \code{ee$batch$Task$start()}) to
#'   transfer the \code{ee$Image} from Earth Engine to the intermediate container
#'   specified in the argument \code{via}.}
#'   \item{2. }{If the argument \code{lazy} is TRUE, the task will not be
#'   monitored. This is useful to lunch several tasks simultaneously and
#'   calls them later using \code{\link{ee_utils_future_value}} or
#'   \code{\link[future:value]{future::value}}. At the end of this step,
#'   the \code{ee$Images} are stored on the path specified in the argument
#'   \code{dsn}.}
#'   \item{3. }{Finally, if the \code{add_metadata}  argument is set to TRUE,
#'   a list containing the following elements will be appended to the \code{dsn} argument.
#'   \itemize{
#'     \item{\bold{if via is "drive":}}
#'       \itemize{
#'         \item{\bold{ee_id: }}{Name of the Earth Engine task.}
#'         \item{\bold{drive_name: }}{Name of the Image in Google Drive.}
#'         \item{\bold{drive_id: }}{Id of the Image in Google Drive.}
#'         \item{\bold{drive_download_link: }}{Download link to the image.}
#'     }
#'   }
#'   \itemize{
#'     \item{\bold{if via is "gcs":}}
#'       \itemize{
#'         \item{\bold{ee_id: }}{Name of the Earth Engine task.}
#'         \item{\bold{gcs_name: }}{Name of the Image in Google Cloud Storage.}
#'         \item{\bold{gcs_bucket: }}{Name of the bucket.}
#'         \item{\bold{gcs_fileFormat: }}{Format of the image.}
#'         \item{\bold{gcs_public_link: }}{Download link to the image.}
#'         \item{\bold{gcs_URI: }}{gs:// link to the image.}
#'     }
#'   }
#'  }
#' }
#'
#' For getting more information about exporting data from Earth Engine, take
#' a look at the
#' \href{https://developers.google.com/earth-engine/guides/exporting}{Google
#' Earth Engine Guide - Export data}.
#' @importFrom crayon green
#' @return If add_metadata is FALSE, \code{ee_imagecollection_to_local} will
#' return a character vector containing the filename of the images downloaded.
#' Otherwise, if add_metadata is TRUE, will return a list with extra information
#' related to the exportation (see details).
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
#' # one by once
#' ic_drive_files_1 <- ee_imagecollection_to_local(
#'   ic = collection,
#'   region = geometry,
#'   scale = 250,
#'   dsn = file.path(tmp, "drive_")
#' )
#'
#' # all at once
#' ic_drive_files_2 <- ee_imagecollection_to_local(
#'   ic = collection,
#'   region = geometry,
#'   scale = 250,
#'   lazy = TRUE,
#'   dsn = file.path(tmp, "drive_")
#' )
#'
#' # From Google Drive to client-side
#' doqq_dsn <- ic_drive_files_2 %>% ee_utils_future_value()
#' sapply(doqq_dsn, '[[', 1)
#' }
#' @export
ee_imagecollection_to_local <- function(ic,
                                        region,
                                        dsn = NULL,
                                        via = "drive",
                                        container = "rgee_backup",
                                        scale = NULL,
                                        maxPixels = 1e9,
                                        lazy = FALSE,
                                        public = TRUE,
                                        add_metadata = TRUE,
                                        timePrefix = TRUE,
                                        quiet = FALSE,
                                        ...) {
  # check packages
  ee_check_packages("ee_imagecollection_to_local", "sf")

  # is image an ee.image.Image?
  if (!any(class(ic) %in% "ee.imagecollection.ImageCollection")) {
    stop("ic argument is not an ee$imagecollection$ImageCollection")
  }

  # is region an ee.geometry.Geometry?
  if (!any(class(region) %in% "ee.geometry.Geometry")) {
    stop("region argument is not an ee$geometry$Geometry")
  }

  ic_names <- NULL
  ic_count <- ic %>%
    ee$ImageCollection$size() %>%
    ee$Number$getInfo()

  # if dsn is null
  if (is.null(dsn)) {
    ic_names <- ic %>%
      ee$ImageCollection$aggregate_array("system:index") %>%
      ee$List$getInfo()
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
    message(
      "dsn is a vector character with the same length of the imagecollection.\n",
      "Length: ", ic_count, "\n",
      "Running: final_names <- dsn"
    )
    ic_names <- dsn
  } else {
    # if dsn is a directory or a character
    if (tryCatch(dir.exists(dsn), error = function(e) FALSE)) {
      ic_names <- ic %>%
        ee$ImageCollection$aggregate_array("system:index") %>%
        ee$List$getInfo()
      ic_names <- sprintf("%s/%s",dsn,ic_names)
    }

    # if dsn is a directory or a character
    if (tryCatch(dir.exists(dirname(dsn)), error = function(e) FALSE)) {
      ic_names <- ic %>%
        ee$ImageCollection$aggregate_array("system:index") %>%
        ee$List$getInfo()
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
    image <- ic %>%
      ee$ImageCollection$toList(count = index + 1, offset = index) %>%
      ee$List$get(0) %>%
      ee$Image()
    if (!quiet) {
      cat(blue$bold("\nDownloading:"), green(ic_names[r_index]))
    }

    img_stars <- ee_as_stars(
      image = image,
      region = region,
      dsn = ic_names[r_index],
      via = via,
      container = container,
      scale = scale,
      maxPixels = maxPixels,
      lazy = lazy,
      public = public,
      add_metadata = add_metadata,
      timePrefix = timePrefix,
      quiet = TRUE
    )

    if (!lazy) {
      if (add_metadata) {
        ic_files[[r_index]] <- list(dsn = img_stars[[1]],
                                    metadata = attr(img_stars, "metadata"))
      } else {
        ic_files[[r_index]] <- img_stars[[1]]
      }
    } else {
      ic_files[[r_index]] <- img_stars
      class(ic_files) <- append(class(ic_files), "ee_imagecollection")
    }
  }
  if (!quiet) {
    cat("\n", rule())
  }
  ic_files
}

#' geometry message
#' @importFrom crayon bold
#' @noRd
ee_geometry_message <- function(region, sf_region = NULL, quiet = FALSE) {
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
  is_geodesic <- region %>%
    ee$Geometry$geodesic() %>%
    ee$ComputedObject$getInfo() %>%
    ee_utils_py_to_r()

  #is evenodd?
  query_params <- unlist(jsonlite::parse_json(ee$Geometry$serialize(region))[["scope"]])
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
