#' Convert an Earth Engine (EE) image in a stars object
#'
#' Convert an ee$Image in a stars object.
#'
#' @param image ee$Image to be converted into a 'stars' object.
#' @param region EE Geometry (ee$Geometry$Polygon) that specifies the region
#' to export. CRS needs to be the same that the argument \code{image}.
#' Otherwise, it will be forced. If not specified, image bounds are taken.
#' @param dsn Character. Output filename. If missing, a temporary file is
#' created.
#' @param via Character. Method to export the image. Three methods are
#' available: "getDownloadURL", "drive", "gcs". For "drive" and "gcs" see details.
#' Use "getDownloadURL" for small images. Default "getDownloadURL".
#' @param container Character. Name of the folder ('drive') or bucket ('gcs')
#' to be exported.
#' @param scale Numeric. Image resolution given in meters per pixel. Defaults
#' to the native resolution of the image.
#' @param maxPixels Numeric. The maximum allowable number of pixels in the exported
#' image. If the exported region covers more pixels than the specified limit in the
#' given projection, the task will fail. Defaults to 100,000,000.
#' @param grid_batch Numeric. Argument used if 'via' is set as "getDownloadURL".
#' The number of pixels to download in each batch without considering the number
#' of bands. Default to 1048576 -(1024*1024).
#' @param lazy Logical. If TRUE, a \code{\link[future:sequential]{
#' future::sequential}} object is created to evaluate the task in the future.
#' See details.
#' @param public Logical. If TRUE, a public link to the image is created.
#' @param add_metadata Add metadata to the stars_proxy object. See details.
#' @param timePrefix Logical. Add current date and time (\code{Sys.time()}) as
#' a prefix to export files. This parameter helps to avoid exported files
#' with the same name. By default TRUE.
#' @param quiet Logical. Suppress info message
#' @param ... Extra exporting argument. See \link{ee_image_to_drive} and
#' \link{ee_image_to_gcs}.
#'
#' @details
#' \code{ee_as_stars} supports the download of \code{ee$Images}
#' by two different options: "drive"
#' (\href{https://CRAN.R-project.org/package=googledrive}{Google Drive}) and "gcs"
#' (\href{https://CRAN.R-project.org/package=googleCloudStorageR}{
#' Google Cloud Storage}). In both cases, \code{ee_as_stars} works as follow:
#' \describe{
#'   \item{1.}{A task is started (i.e. \code{ee$batch$Task$start()}) to
#'   move the \code{ee$Image} from Earth Engine to the intermediate container
#'   specified in the argument \code{via}.}
#'   \item{2.}{If the argument \code{lazy} is TRUE, the task will not be
#'   monitored. This is useful to lunch several tasks simultaneously and
#'   calls them later using \code{\link{ee_utils_future_value}} or
#'   \code{\link[future:value]{future::value}}. At the end of this step,
#'   the \code{ee$Image} is stored on the path specified in the argument
#'   \code{dsn}.}
#'   \item{3.}{Finally, if the argument \code{add_metadata} is TRUE, a list
#'   with the following elements is added to the stars-proxy object.
#'   \describe{
#'     \item{\bold{if via is "drive":}}{
#'       \describe{
#'         \item{\bold{ee_id:}}{Name of the Earth Engine task.}
#'         \item{\bold{drive_name:}}{Name of the Image in Google Drive.}
#'         \item{\bold{drive_id:}}{Id of the Image in Google Drive.}
#'         \item{\bold{drive_download_link:}}{Download link to the image.}
#'     }}
#'   }
#'   \describe{
#'     \item{\bold{if via is "gcs":}}{
#'       \describe{
#'         \item{\bold{ee_id:}}{Name of the Earth Engine task.}
#'         \item{\bold{gcs_name:}}{Name of the Image in Google Cloud Storage.}
#'         \item{\bold{gcs_bucket:}}{Name of the bucket.}
#'         \item{\bold{gcs_fileFormat:}}{Format of the image.}
#'         \item{\bold{gcs_public_link:}}{Download link to the image.}
#'         \item{\bold{gcs_URI:}}{gs:// link to the image.}
#'     }}
#'   }
#'  }
#' }
#' Run \code{attr(stars, "metadata")} to get the list.
#'
#' For getting more information about exporting data from Earth Engine, take
#' a look at the
#' \href{https://developers.google.com/earth-engine/guides/exporting}{Google
#' Earth Engine Guide - Export data}.
#' @return A stars-proxy object
#' @family image download functions
#' @export
#' @examples
#' \dontrun{
#' library(rgee)
#'
#' ee_Initialize(drive = TRUE, gcs = TRUE)
#' ee_user_info()
#'
#' # Define an image.
#' img <- ee$Image("LANDSAT/LC08/C01/T1_SR/LC08_038029_20180810")$
#'   select(c("B4", "B3", "B2"))$
#'   divide(10000)
#'
#' # OPTIONAL display it using Map
#' Map$centerObject(eeObject = img)
#' Map$addLayer(eeObject = img, visParams = list(max = 0.4,gamma=0.1))
#'
#' # Define an area of interest.
#' geometry <- ee$Geometry$Rectangle(
#'   coords = c(-110.8, 44.6, -110.6, 44.7),
#'   proj = "EPSG:4326",
#'   geodesic = FALSE
#' )
#'
#' ## getDownloadURL - Method 01 (for small images)
#' img_02 <- ee_as_stars(
#'   image = img,
#'   region = geometry,
#'   scale = 10
#' )
#'
#' ## drive - Method 02
#' # Simple
#' img_02 <- ee_as_stars(
#'   image = img,
#'   region = geometry,
#'   via = "drive"
#' )
#'
#' # Lazy
#' img_02 <- ee_as_stars(
#'   image = img,
#'   region = geometry,
#'   via = "drive",
#'   lazy = TRUE
#' )
#'
#' img_02_result <- img_02 %>% ee_utils_future_value()
#' attr(img_02_result, "metadata") # metadata
#'
#' ## gcs - Method 03
#' # Simple
#' img_03 <- ee_as_stars(
#'   image = img,
#'   region = geometry,
#'   container = "rgee_dev",
#'   via = "gcs"
#' )
#'
#' # Lazy
#' img_03 <- ee_as_stars(
#'   image = img,
#'   region = geometry,
#'   container = "rgee_dev",
#'   lazy = TRUE,
#'   via = "gcs"
#' )
#'
#' img_03_result <- img_03 %>% ee_utils_future_value()
#' attr(img_03_result, "metadata") # metadata
#'
#' # OPTIONAL: clean containers
#' # ee_clean_container(name = "rgee_backup", type = "drive")
#' # ee_clean_container(name = "rgee_dev", type = "gcs")
#' }
#' @export
ee_as_stars <- function(image,
                        region = NULL,
                        dsn = NULL,
                        via = "drive",
                        container = "rgee_backup",
                        scale = NULL,
                        maxPixels = 1e9,
                        grid_batch = 1024*1024,
                        lazy = FALSE,
                        public = FALSE,
                        add_metadata = TRUE,
                        timePrefix = TRUE,
                        quiet = FALSE,
                        ...) {
  ee_check_packages("ee_as_stars", c("stars", "sf", "future", "terra"))

  if (via == "getDownloadURL") {
    if (is.null(scale)) {
      stop("getDownloadURL needs to define the scale argument.")
    }

    if (is.null(dsn)) {
      dsn <- tempfile(fileext = ".tif")
    }

    rast_obj <- ee_image_local_getDownloadURL(
      image = image,
      dsn = dsn,
      quiet = quiet,
      scale = scale,
      grid_batch = grid_batch,
      export = "stars",
      format = "GEO_TIFF",
      geometry = region
    )
    return(rast_obj)
  }

  # 1. From Earth Engine to the container (drive or gcs)
  # Initialize the task! depending of the argument "via", the arguments
  # of ee_image_to_drive or ee_image_to_gcs could be passed.
  ee_task <- ee_init_task(
    image = image,
    region = region,
    dsn = dsn,
    via = via,
    scale = scale,
    maxPixels = maxPixels,
    container = container,
    timePrefix = timePrefix,
    quiet = quiet,
    ...
  )

  user_email <- ee_get_current_email()

  to_evaluate <- function() {
    # 2. From the container to the client-side.
    img_dsn <- ee_image_local(
      task = ee_task$task,
      user_email = user_email,
      dsn = ee_task$dsn,
      via = via,
      metadata = add_metadata,
      public = public,
      quiet = quiet
    )

    # Copy band names
    band_names <- image %>%
      ee$Image$bandNames() %>%
      ee$List$getInfo()

    # Create a proxy-star object
    ee_read_stars(img_dsn$dsn, band_names, img_dsn$metadata)
  }


  if (lazy) {
    prev_plan <- future::plan(future::sequential, .skip = TRUE)
    on.exit(future::plan(prev_plan, .skip = TRUE), add = TRUE)
    future::future({
      to_evaluate()
    }, lazy = TRUE)
  } else {
    to_evaluate()
  }
}



#' Convert an Earth Engine (EE) image in a SpatRaster object
#'
#' Convert an ee$Image in a SpatRaster object
#'
#' @param image ee$Image to be converted into a SpatRaster object.
#' @param region EE Geometry (ee$Geometry$Polygon) which specifies the region
#' to export. CRS needs to be the same that the argument \code{image}.
#' Otherwise, it will be forced. If not specified, image bounds are taken.
#' @param dsn Character. Output filename. If missing, a temporary file is
#' created.
#' @param via Character. Method to export the image. Three methods are
#' implemented: "getDownloadURL", "drive", "gcs". For "drive" and "gcs" see details.
#' Use "getDownloadURL" for small images.
#' @param container Character. Name of the folder ('drive') or bucket ('gcs')
#' to be exported.
#' @param scale Numeric. The resolution in meters per pixel. Defaults
#' to the native resolution of the image.
#' @param maxPixels Numeric. The maximum allowable number of pixels in the exported
#' image. If the exported region covers more pixels than the specified limit in the
#' given projection, the task will fail. Defaults to 100,000,000.
#' @param grid_batch Numeric. Argument used if via is set as "getDownloadURL". The number of
#' pixels to download in each batch without considering the number of bands. Default
#' to 1048576 -(1024*1024).
#' @param lazy Logical. If TRUE, a \code{\link[future:sequential]{
#' future::sequential}} object is created to evaluate the task in the future.
#' See details.
#' @param public Logical. If TRUE, a public link to the image is created.
#' @param add_metadata Add metadata to the stars_proxy object. See details.
#' @param timePrefix Logical. Add current date and time (\code{Sys.time()}) as
#' a prefix to files to export. This parameter helps to avoid exported files
#' with the same name. By default TRUE.
#' @param quiet Logical. Suppress info message
#' @param ... Extra exporting argument. See \link{ee_image_to_drive} and
#' \link{ee_image_to_gcs}.
#' @details
#' \code{ee_as_rast} supports the download of \code{ee$Images} using: "drive"
#' (\href{https://CRAN.R-project.org/package=googledrive}{Google Drive}) and "gcs"
#' (\href{https://CRAN.R-project.org/package=googleCloudStorageR}{
#' Google Cloud Storage}). In both cases, \code{ee_as_rast} performs as follows:
#' \describe{
#'   \item{1. }{A task is started (i.e., \code{ee$batch$Task$start()}) to
#'   move the \code{ee$Image} from Earth Engine to the intermediate container
#'   specified in the argument \code{via}.}
#'   \item{2. }{If the argument \code{lazy} is TRUE, the task is not be
#'   monitored. This is useful to lunch several tasks simultaneously and
#'   calls them later using \code{\link{ee_utils_future_value}} or
#'   \code{\link[future:value]{future::value}}. At the end of this step,
#'   the \code{ee$Image} is stored on the path specified in the argument
#'   \code{dsn}.}
#'   \item{3. }{Finally, if the argument \code{add_metadata} is TRUE, a list
#'   with the following elements are added to the stars-proxy object.
#'   \describe{
#'     \item{\bold{if via is "drive":}}{
#'       \describe{
#'         \item{\bold{ee_id: }}{Name of the Earth Engine task.}
#'         \item{\bold{drive_name: }}{Name of the Image in Google Drive.}
#'         \item{\bold{drive_id: }}{Id of the Image in Google Drive.}
#'         \item{\bold{drive_download_link: }}{Download link to the image.}
#'     }}
#'   }
#'   \describe{
#'     \item{\bold{if via is "gcs":}}{
#'       \describe{
#'         \item{\bold{ee_id: }}{Name of the Earth Engine task.}
#'         \item{\bold{gcs_name: }}{Name of the Image in Google Cloud Storage.}
#'         \item{\bold{gcs_bucket: }}{Name of the bucket.}
#'         \item{\bold{gcs_fileFormat: }}{Format of the image.}
#'         \item{\bold{gcs_public_link: }}{Download link to the image.}
#'         \item{\bold{gcs_URI: }}{gs:// link to the image.}
#'     }}
#'   }
#'
#'   Run \code{attr(stars, "metadata")} to get the list.
#'  }
#' }
#'
#' For getting more information about exporting data from Earth Engine, take
#' a look at the
#' \href{https://developers.google.com/earth-engine/guides/exporting}{Google
#' Earth Engine Guide - Export data}.
#' @return A SpatRaster object
#' @family image download functions
#' @examples
#' \dontrun{
#' library(rgee)
#'
#' ee_Initialize(drive = TRUE, gcs = TRUE)
#' ee_user_info()
#'
#' # Define an image.
#' img <- ee$Image("LANDSAT/LC08/C01/T1_SR/LC08_038029_20180810")$
#'   select(c("B4", "B3", "B2"))$
#'   divide(10000)
#'
#' # OPTIONAL display it using Map
#' Map$centerObject(eeObject = img)
#' Map$addLayer(eeObject = img, visParams = list(max = 0.4,gamma=0.1))
#'
#' # Define an area of interest.
#' geometry <- ee$Geometry$Rectangle(
#'   coords = c(-110.8, 44.6, -110.6, 44.7),
#'   proj = "EPSG:4326",
#'   geodesic = FALSE
#' )
#'
#' ## getDownloadURL - Method 01 (for small images)
#' img_02 <- ee_as_stars(
#'   image = img,
#'   region = geometry,
#'   scale = 10
#' )
#'
#' ## drive - Method 02
#' # Simple
#' img_02 <- ee_as_rast(
#'   image = img,
#'   region = geometry,
#'   via = "drive"
#' )
#'
#' # Lazy
#' img_02 <- ee_as_rast(
#'   image = img,
#'   region = geometry,
#'   via = "drive",
#'   lazy = TRUE
#' )
#'
#' img_02_result <- img_02 %>% ee_utils_future_value()
#' attr(img_02_result, "metadata") # metadata
#'
#' ## gcs - Method 03
#' # Simple
#' img_03 <- ee_as_rast(
#'  image = img,
#'  region = geometry,
#'  container = "rgee_dev",
#'  via = "gcs"
#' )
#'
#' # Lazy
#' img_03 <- ee_as_rast(
#'  image = img,
#'  region = geometry,
#'  container = "rgee_dev",
#'  lazy = TRUE,
#'  via = "gcs"
#' )
#'
#' img_03_result <- img_03 %>% ee_utils_future_value()
#' attr(img_03_result, "metadata") # metadata
#'
#' # OPTIONAL: clean containers
#' # ee_clean_container(name = "rgee_backup", type = "drive")
#' # ee_clean_container(name = "rgee_dev", type = "gcs")
#' }
#' @export
ee_as_rast <- function(image,
                       region = NULL,
                       dsn = NULL,
                       via = "drive",
                       container = "rgee_backup",
                       scale = NULL,
                       maxPixels = 1e9,
                       grid_batch = 1024*1024,
                       lazy = FALSE,
                       public = FALSE,
                       add_metadata = TRUE,
                       timePrefix = TRUE,
                       quiet = FALSE,
                       ...) {

  ee_check_packages("ee_as_stars", c("terra", "sf", "future"))


  if (via == "getDownloadURL") {
    if (is.null(scale)) {
      stop("getDownloadURL needs to define the scale argument.")
    }

    if (is.null(dsn)) {
      dsn <- tempfile(fileext = ".tif")
    }

    # Download data
    counter <- 1
    while(TRUE) {
        rast_obj <- tryCatch(
          expr = {
              ee_image_local_getDownloadURL(
                image = image,
                dsn = dsn,
                quiet = quiet,
                scale = scale,
                grid_batch = grid_batch,
                export = "terra",
                format = "GEO_TIFF",
                geometry = region
              )
          }, error = function(e) {
              Sys.sleep(1)
              if (counter > 4) {
                  stop(e)
              }
              counter <- counter + 1
              FALSE
          }
        )

        if (inherits(rast_obj, "SpatRaster")) {
            break
        }
    }

    return(rast_obj)
  }

  ee_task <- ee_init_task(
    image = image,
    region = region,
    dsn = dsn,
    via = via,
    scale = scale,
    container = container,
    maxPixels = maxPixels,
    timePrefix = timePrefix,
    quiet = quiet,
    ...
  )

  user_email <- ee_get_current_email()

  to_evaluate <- function() {
    # 2. From the container to the client-side.
    img_dsn <- ee_image_local(
      task = ee_task$task,
      user_email = user_email,
      dsn = ee_task$dsn,
      via = via,
      metadata = add_metadata,
      public = public,
      quiet = quiet
    )

    # Copy band names
    band_names <- image %>%
      ee$Image$bandNames() %>%
      ee$List$getInfo()

    # Create a proxy-star object
    ee_read_rast(img_dsn$dsn, band_names, img_dsn$metadata)
  }

  if (lazy) {
    prev_plan <- future::plan(future::sequential, .skip = TRUE)
    on.exit(future::plan(prev_plan, .skip = TRUE), add = TRUE)
    future::future({
      to_evaluate()
    }, lazy = TRUE)
  } else {
    to_evaluate()
  }
}


#' Passing an Earth Engine Image to Local
#' @noRd
ee_init_task <- function(image,
                         region,
                         dsn = NULL,
                         via = "drive",
                         scale = NULL,
                         maxPixels = 1e9,
                         timePrefix = TRUE,
                         container = "rgee_backup",
                         quiet = FALSE,
                         ...) {
  ee_check_packages("ee_init_task", c("sf", "jsonlite", "stars"))

  # is image an ee.image.Image?
  if (!any(class(image) %in% "ee.image.Image")) {
    stop("x argument is not an ee$image$Image")
  }

  # is region an ee.geometry.Geometry?
  if (!any(class(region) %in% c("ee.geometry.Geometry", "NULL"))) {
    stop("region argument is not an ee$geometry$Geometry")
  }

  # Get bandnames
  band_names <- image %>%
    ee$Image$bandNames() %>%
    ee$List$getInfo()

  if (via == "drive") {
    ee_init_task_drive(image, region, dsn, scale, maxPixels,
                       timePrefix, container, quiet, ...)
  } else if (via == "gcs") {
    ee_init_task_gcs(image, region, dsn, scale, maxPixels,
                     timePrefix, container, quiet, ...)
  } else {
    stop("via argument invalid")
  }
}

#' Create a Export task to GD
#' @noRd
ee_init_task_drive <- function(image, region, dsn, scale, maxPixels, timePrefix,
                               container, quiet, ...) {

  extras <- list(...)

  # folder is container
  if (any(names(extras) %in%  "folder")) {
    stop(
      "To specify the folder where to export files",
      " use the argument container instead of folder."
    )
  }

  # Have you loaded the necessary credentials?
  # Relevant for either drive or gcs.
  ee_user <- ee_exist_credentials()

  if (is.null(dsn)) {
    # Getting image ID if it is exist
    image_id <- tryCatch(
      expr = {
        image %>%
          ee$Image$get("system:id") %>%
          ee$ComputedObject$getInfo() %>%
          basename()
      }, error = function(e) "noid_image"
    )
    if (is.null(image_id)) {
      image_id <- "noid_image"
    }
    dsn <- sprintf("%s/%s.tif",tempdir(), image_id)
  } else {
    image_id <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(dsn))
  }

  # Create description (Human-readable name of the task)
  # Relevant for either drive or gcs.
  time_format <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
  ee_description <- paste0("rgeeImage_", time_format)
  if (timePrefix) {
    file_name <- paste0(image_id, "_", time_format)
  } else {
    file_name <- image_id
  }

  # Are GD credentials loaded?
  if (is.na(ee_user$drive_cre)) {
    drive_credential <- ee_create_credentials_drive(ee_user$email)
    ee_save_credential(pdrive = drive_credential)
    # ee_Initialize(user = ee_user$email, drive = TRUE)
    message(
      "\nNOTE: Google Drive credentials were not loaded.",
      " Running ee_Initialize(user = '", ee_user$email, "', drive = TRUE)",
      " to fix."
    )
  }

  # region parameter display
  if (!is.null(region)) {
    ee_geometry_message(region, quiet = quiet)
  }

  # From Google Earth Engine to Google Drive
  img_task <- ee_image_to_drive(
    image = image,
    timePrefix = FALSE,
    description = ee_description,
    scale = scale,
    folder = container,
    fileFormat = "GEO_TIFF",
    region = region,
    maxPixels = maxPixels,
    fileNamePrefix = file_name,
    ...
  )

  # download parameter display
  if (!quiet) {
    cat(
      bold("\n- download parameters (Google Drive)\n"),
      bold("Image ID    :"), image_id, "\n",
      bold("Google user :"), ee_user$email, "\n",
      bold("Folder name :"), container, "\n",
      bold("Date        :"), time_format, "\n"
    )
  }
  ee$batch$Task$start(img_task)
  list(task = img_task, dsn = dsn)
}


#' Create a Export task to GCS
#' @noRd
ee_init_task_gcs <- function(image, region, dsn, scale, maxPixels,
                             timePrefix, container, quiet, ...) {
  extras <- list(...)

  # bucket is container
  if (any(names(extras) %in%  "bucket")) {
    stop(
      "To specify the bucket where to export files",
      " use the argument container instead of bucket."
    )
  }

  # Have you loaded the necessary credentials?
  # Relevant for either drive or gcs.
  ee_user <- ee_exist_credentials()

  if (is.na(ee_user$gcs_cre)) {
    gcs_credential <- ee_create_credentials_gcs(ee_user$email)
    ee_save_credential(pgcs = gcs_credential$path)
    message(
      "\nGoogle Cloud Storage credentials were not loaded.",
      " Running ee_Initialize(user = '", ee_user$email,"', gcs = TRUE)",
      " to fix."
    )
  }

  if (is.null(container)) {
    stop("Cloud Storage bucket was not defined")
  } else {
    tryCatch(
      expr = googleCloudStorageR::gcs_get_bucket(container),
      error = function(e) {
        stop(sprintf("The %s bucket was not found.", container))
      }
    )
  }

  # region parameter display
  if (!is.null(region)) {
    ee_geometry_message(region, quiet = quiet)
  }

  # Getting image ID if it is exist
  if (is.null(dsn)) {
    # Getting image ID if it is exist
    image_id <- tryCatch(
      expr = {
        image %>%
          ee$Image$get("system:id") %>%
          ee$ComputedObject$getInfo() %>%
          basename()
      }, error = function(e) "noid_image"
    )
    if (is.null(image_id)) {
      image_id <- "noid_image"
    }
    dsn <- sprintf("%s/%s.tif",tempdir(), image_id)
  } else {
    image_id <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(dsn))
  }

  # Relevant for either drive or gcs.
  time_format <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
  ee_description <- paste0("rgeeImage_", time_format)
  if (timePrefix) {
    file_name <- paste0(image_id, "_", time_format)
  } else {
    file_name <- image_id
  }

  # From Earth Engine to Google Cloud Storage
  img_task <- ee_image_to_gcs(
    image = image,
    description = ee_description,
    timePrefix = FALSE,
    bucket = container,
    fileFormat = "GEO_TIFF",
    region = region,
    maxPixels = maxPixels,
    scale = scale,
    fileNamePrefix = file_name,
    ...
  )

  # download parameter display
  if (!quiet) {
    cat(
      bold("\n- download parameters (Google Cloud Storage)\n"),
      bold("Image ID    :"), image_id, "\n",
      bold("Google user :"), ee_user$email, "\n",
      bold("Bucket name :"), container, "\n",
      bold("Date        :"), time_format, "\n"
    )
  }
  img_task$start()
  list(task = img_task, dsn = dsn)
}

#' Passing an Earth Engine Image to Local
#' @noRd
ee_image_local <- function(task, user_email, dsn, via, metadata, public, quiet) {
  if (via == "drive") {
    ee_create_credentials_drive(user_email)
    ee_image_local_drive(task, dsn, metadata, public, quiet)
  } else if (via == "gcs") {
    ee_create_credentials_gcs(user_email)
    ee_image_local_gcs(task, dsn, metadata, public, quiet)
  } else {
    stop("via argument invalid")
  }
}


#' Passing an Earth Engine Image from GD to Local
#' @noRd
ee_image_local_drive <- function(task, dsn, metadata, public, quiet) {
  ee_monitoring(task = task, quiet = quiet, max_attempts = Inf)
  # From Google Drive to local
  if (isFALSE(quiet)) {
    cat('Moving image from Google Drive to Local ... Please wait  \n')
  }
  dsn <- ee_drive_to_local(
    task = task,
    dsn = dsn,
    consider = 'all',
    metadata = metadata,
    public = public,
    quiet = quiet
  )
  if (is.character(dsn)) {
    dsn <- list(dsn = dsn)
  }
  invisible(dsn)
}


#' Passing an Earth Engine Image from GCS to Local
#' @noRd
ee_image_local_gcs <- function(task, dsn, metadata, public, quiet) {
  # earth engine monitoring
  ee_monitoring(task = task, quiet = quiet, max_attempts = Inf)

  # From Google Cloud Storage to local
  if(isFALSE(quiet)) {
    cat('Moving image from GCS to Local ... Please wait  \n')
  }

  dsn <- ee_gcs_to_local(
    task = task,
    dsn = dsn,
    metadata = metadata,
    public = public,
    quiet = quiet
  )

  if (is.character(dsn)) {
    dsn <- list(dsn = dsn)
  }

  invisible(dsn)
}

#' Approximate size of an EE Image object
#'
#' Get the approximate number of rows, cols, and size of a single-band
#' Earth Engine Image.
#'
#' @param image Single-band EE Image object.
#' @param getsize Logical. If TRUE, the function will estimate the size
#' of the object
#' @param band_metadata A list with image properties. If NULL it will be  automatically generated.
#' @param compression_ratio Numeric. Measurement of the relative data size
#' reduction produced by a data compression algorithm (ignored if
#' \code{getsize} is FALSE). By default is 20.
#' @param quiet Logical. Suppress info message
#' @return A list containing information about the number of rows (nrow),
#' number of columns (ncol), total number of pixels (total_pixel), and image
#' size (image_size).
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_Initialize()
#'
#' # World SRTM
#' srtm <- ee$Image("CGIAR/SRTM90_V4")
#' ee_image_info(srtm)
#'
#' # Landast8
#' l8 <- ee$Image("LANDSAT/LC08/C01/T1_SR/LC08_038029_20180810")$select("B4")
#' ee_image_info(l8)
#' }
#' @export
ee_image_info <- function(image,
                          band_metadata = NULL,
                          getsize = TRUE,
                          compression_ratio = 20,
                          quiet = FALSE) {
  #check packages
  ee_check_packages("ee_image_info", "sf")

  band_length <- length(image$bandNames()$getInfo())

  if (is.null(band_metadata)) {
    band_info <- ee$Image$getInfo(image)
    band_properties <- band_info$properties
    band_metadata <- band_info$bands[[1]]
  }

  # if (band_length != 1) {
  #   stop("ee_image_info needs that image only has one band.")
  # }
  img_proj <- tryCatch(
    expr = image$projection()$getInfo(),
    error = function(e) {
      message(paste0(e$message, " Trying only taking the first band...."))
      image$select(0)$projection()$getInfo()
    }
  )

  geotransform <- unlist(img_proj$transform)
  img_proj_wkt <- sf::st_crs(ee_utils_get_crs(band_metadata$crs))

  img_totalarea <- ee_as_sf(image$geometry()) %>%
    sf::st_transform(img_proj_wkt)

  bbox <- img_totalarea %>%
    sf::st_bbox() %>%
    as.numeric()

  x_diff <- bbox[3] - bbox[1]
  y_diff <- bbox[4] - bbox[2]
  x_npixel <- ceiling(abs(x_diff / geotransform[1]))
  y_npixel <- ceiling(abs(y_diff / geotransform[5]))
  total_pixel <- abs(as.numeric(x_npixel * y_npixel))

  if (!quiet) {
    cat(bold("Image Rows       :"), x_npixel, "\n")
    cat(bold("Image Cols       :"), y_npixel, "\n")
    cat(bold("Number of Pixels :"), format(total_pixel, scientific = FALSE), "\n")
  }

  # Obtain the size of an ee.Image
  if (isFALSE(getsize)) {
    invisible(
      list(
        nrow = x_npixel,
        ncol = y_npixel,
        total_pixel = total_pixel,
        image_wkt = img_proj_wkt,
        image_bounds = img_totalarea
      )
    )
  } else {
    image_id <- ee_utils_py_to_r(image$get("system:id")$getInfo())
    if (!is.null(image_id)) {
      image_size <- ee_manage_asset_size(image_id, quiet = TRUE) / band_length
      if (length(image_size) == 0) {
        image_size <- ee_image_msize(image, total_pixel, compression_ratio)
      }
    } else {
      image_size <-ee_image_msize(image, total_pixel, compression_ratio)
    }
    if (!quiet) {
      cat(bold("Image Size       :"), ee_humansize(image_size), "\n")
    }
    invisible(
      list(
        nrow = x_npixel,
        ncol = y_npixel,
        total_pixel = total_pixel,
        image_size = image_size,
        image_wkt = img_proj_wkt,
        image_bounds = img_totalarea
      )
    )
  }
}

#' Get manually the image size
#' @noRd
ee_image_msize <- function(image, total_pixel, compression_ratio) {
  bandtypes_info <- image$bandTypes()$getInfo()
  img_types <- unlist(bandtypes_info)
  band_types <- img_types[grepl("precision", names(img_types))]
  band_precision <- vapply(band_types, ee_get_typeimage_size, 0)
  number_of_bytes <- total_pixel * band_precision / compression_ratio
  sum(number_of_bytes)
}

#' helper function to read raster (ee_read_stars)
#' @noRd
ee_read_stars <- function(img_dsn, band_names, metadata) {
  img_stars <- stars::read_stars(img_dsn, proxy = TRUE)
  attr(img_stars, "metadata") <- metadata
  if (length(stars::st_dimensions(img_stars)) < 3) {
    img_stars
  } else {
    stars::st_set_dimensions(img_stars, 3, values = band_names)
  }
}

#' helper function to read raster (ee_as_raster)
#' @noRd
ee_read_raster <- function(img_dsn, band_names, metadata) {
  if (length(img_dsn) > 1) {
    message("NOTE: To avoid memory excess problems, ee_as_raster will",
            " not build Raster objects for large images.")
    img_dsn
  } else {
    dsn_raster <- raster::stack(img_dsn)
    dsn_raster@history <- list(metadata = metadata)
    dsn_raster
  }
}


#' helper function to read raster (ee_as_raster)
#' @noRd
ee_read_rast <- function(img_dsn, band_names, metadata) {
  if (length(img_dsn) > 1) {
    message(
      "NOTE: To avoid memory excess problems, ee_as_rast will",
      " not build Raster objects for large images."
    )
    img_dsn
  } else {
    dsn_raster <- terra::rast(img_dsn)
    attr(dsn_raster, "metadata") <- list(metadata = metadata)
    dsn_raster
  }
}

#' Get the current image
#' @noRd
ee_get_current_email <- function() {
  oauth_func_path <- system.file("python/ee_utils.py", package = "rgee")
  utils_py <- ee_source_python(oauth_func_path)
  ee_path <- ee_utils_py_to_r(utils_py$ee_path())
  read.table(file = sprintf("%s/rgee_sessioninfo.txt", ee_path),
             header = TRUE,
             stringsAsFactors = FALSE)[["email"]]
}




#' Download a raster using getDownloadURL
#' @param image The image to download
#' @param dsn The destination file name
#' @param quiet If TRUE, suppress messages,
#' @param scale The scale to download
#' @param grid_batch The side of a square matrix. It is used to split the image in n parts.
#' @param export The export type. It can be "terra", "stars" or "file". If "file" is used,
#' the function returns the file name.
#' @param geometry The geometry to download. It can be a sf object or a ee.Geometry object.
#' @noRd
ee_image_local_getDownloadURL <- function(
    image,
    dsn,
    geometry,
    scale,
    grid_batch = 1024*1024,
    export = "terra",
    format = "GEO_TIFF",
    quiet=FALSE,
    ...
) {
  # Check if geometry if sf or ee
  if (inherits(geometry, "sfc")) {
    geom_sf <- geometry
    geom_ee <- sf_as_ee(geom_sf)
  } else if(inherits(geometry, "sf")) {
    geom_sf <- geometry[["geometry"]]
    geom_ee <- sf_as_ee(geom_sf)
  } else if(inherits(geometry, "ee.geometry.Geometry")) {
    geom_ee <- geometry
    geom_sf <- ee_as_sf(geom_ee)
  } else {
    stop("Geometry must be either an sf object or an ee.geometry.Geometry object.")
  }

  # If the area is large, make a grid of tiles
  geom_area <- sf::st_area(geom_sf) %>% as.numeric()
  large_condition <- geom_area  / (grid_batch * (scale**2))

  if (large_condition <= 1) {
    ee_image_local_getDownloadURL_nocheck(
      image = image,
      geom_ee = geom_ee,
      scale = scale,
      dsn = dsn,
      grid_batch = grid_batch,
      export = export,
      format = format
    )
  } else {
    # Split the total geometry in small patches
    nsplits <- ceiling(large_condition)
    geom_sf_batch <- sf::st_make_grid(geom_sf, n = c(nsplits, 1))
    crs_fullimg <- sf::st_crs(geom_sf_batch)
    crs_fullimg_epsg <- crs_fullimg$epsg

    if (is.na(crs_fullimg_epsg)) {
      if (grepl("4326", crs_fullimg$input)) {
        crs_fullimg_epsg_str <- "EPSG:4326"
      }  else {
        stop("sf::st_crs can not determinate the EPSG.")
      }
    } else {
      crs_fullimg_epsg_str <- sprintf("EPSG:%s", crs_fullimg_epsg)
    }

    # Create n tmp files
    tmpfiles <- sapply(
      X = 1:nsplits,
      FUN = function(x) {
        tempfile(
          pattern = sprintf("rgee_getDownloadURL_%02d_", x),
          fileext = ".tif"
        )
      })

    counter <- 1
    for (index in 1:nsplits) {
      if (!quiet) {
        taskname_b <- basename(tmpfiles[index])
        taskname_id <- gsub("rgee_getDownloadURL_|\\.tif$", "", taskname_b)
        taskname_f <- sprintf("EETask: T%s", taskname_id)
        cat(sprintf("\r %s Downloading image patches ... [%s/%s]", taskname_f, counter, nsplits))
        counter <- counter + 1
      }

      ee_image_local_getDownloadURL_nocheck(
        image = image,
        geom_ee = sf_as_ee(geom_sf_batch[index], proj = crs_fullimg_epsg_str),
        scale = scale,
        dsn = tmpfiles[index],
        grid_batch = grid_batch,
        export = "file",
        format = format
      )
    }

    fullimg <- terra::merge(terra::sprc(tmpfiles))
    terra::writeRaster(x = fullimg, filename = dsn, overwrite = TRUE)

    # merge
    if (export  == "terra") {
      fullimg
    } else if (export  == "file") {
      dsn
    } else if (export == "stars") {
      stars::read_stars(dsn)
    } else {
      stop("export argument must be either 'terra' or 'stars'")
    }
  }
}


#' Auxiliary function to ee_image_local_getDownloadURL, it helps to download a
#' specific image patch.
#' @param ... Additional arguments to pass to download.file.
#' @noRd
ee_image_local_getDownloadURL_nocheck <- function(
    image,
    geom_ee,
    scale,
    dsn = NULL,
    grid_batch = 1024,
    export = "terra",
    format = "GEO_TIFF",
    ...
) {

  # if dsn is NULL, return a temp file
  if (is.null(dsn)) {
    dsn <- tempfile()
  }

  local_id <- image %>%
    ee$Image$getDownloadURL(
      list(
        scale = scale,
        format = "GEO_TIFF",
        region = geom_ee,
        filePerBand = FALSE
      )
    )

  # Download the image
  tryCatch(
    expr = download.file(url = local_id, destfile = dsn, quiet = TRUE, ...),
    error = function(e) {
      message(e)
      stop(
        "An error occurred while downloading the file. If GEE return a memory limit error message",
        ", try download using a smaller grid_batch. Current grid_batch is ", grid_batch, "."
      )
    }
  )

  # Export the results as a raster object
  if (export == "terra") {
    terra::rast(dsn)
  } else if (export == "stars") {
    stars::read_stars(dsn)
  } else if (export == "file") {
    dsn
  } else {
    stop("export argument must be either 'terra' or 'stars'")
  }
}
