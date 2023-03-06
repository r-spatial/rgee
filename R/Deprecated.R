#' Convert an Earth Engine (EE) image in a raster object
#'
#' Convert an ee$Image in a raster object
#'
#' @param image ee$Image to be converted into a raster object.
#' @param region EE Geometry (ee$Geometry$Polygon) which specifies the region
#' to export. CRS needs to be the same that the argument \code{image}.
#' Otherwise, it will be forced. If not specified, image bounds are taken.
#' @param dsn Character. Output filename. If missing, a temporary file is
#' created.
#' @param via Character. Method to export the image. Two methods are
#' implemented: "drive", "gcs". See details.
#' @param container Character. Name of the folder ('drive') or bucket ('gcs')
#' to be exported.
#' @param scale Numeric. The resolution in meters per pixel. Defaults
#' to the native resolution of the image.
#' @param maxPixels Numeric. The maximum allowed number of pixels in the
#' exported image. The task will fail if the exported region covers
#' more pixels in the specified projection. Defaults to 100,000,000.
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
#' \code{ee_as_raster} supports the download of \code{ee$Images}
#' by two different options: "drive"
#' (\href{https://CRAN.R-project.org/package=googledrive}{Google Drive}) and "gcs"
#' (\href{https://CRAN.R-project.org/package=googleCloudStorageR}{
#' Google Cloud Storage}). In both cases, \code{ee_as_stars} works as follow:
#' \itemize{
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
#'   Run \code{raster@history@metadata} to get the list.
#'  }
#' }
#'
#' For getting more information about exporting data from Earth Engine, take
#' a look at the
#' \href{https://developers.google.com/earth-engine/guides/exporting}{Google
#' Earth Engine Guide - Export data}.
#' @return A RasterStack object
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
#' ## drive - Method 01
#' # Simple
#' img_02 <- ee_as_raster(
#'   image = img,
#'   region = geometry,
#'   via = "drive"
#' )
#'
#' # Lazy
#' img_02 <- ee_as_raster(
#'   image = img,
#'   region = geometry,
#'   via = "drive",
#'   lazy = TRUE
#' )
#'
#' img_02_result <- img_02 %>% ee_utils_future_value()
#' img_02_result@history$metadata # metadata
#'
#' ## gcs - Method 02
#' # Simple
#' img_03 <- ee_as_raster(
#'  image = img,
#'  region = geometry,
#'  container = "rgee_dev",
#'  via = "gcs"
#' )
#'
#' # Lazy
#' img_03 <- ee_as_raster(
#'  image = img,
#'  region = geometry,
#'  container = "rgee_dev",
#'  lazy = TRUE,
#'  via = "gcs"
#' )
#'
#' img_03_result <- img_03 %>% ee_utils_future_value()
#' img_03_result@history$metadata # metadata
#'
#' # OPTIONAL: clean containers
#' # ee_clean_container(name = "rgee_backup", type = "drive")
#' # ee_clean_container(name = "rgee_dev", type = "gcs")
#' }
#' @export
ee_as_raster <- function(image,
                         region = NULL,
                         dsn = NULL,
                         via = "drive",
                         container = "rgee_backup",
                         scale = NULL,
                         maxPixels = 1e9,
                         lazy = FALSE,
                         public = FALSE,
                         add_metadata = TRUE,
                         timePrefix = TRUE,
                         quiet = FALSE,
                         ...) {
  msg <- c(
    "ee_as_raster will be removed in version 1.2.0. Please note that",
    " you can use the ee_as_rast instead, which is compatible with terra packages."
  )

  .Deprecated(
    msg =paste0(msg, collapse = "")
  )

  ee_check_packages("ee_as_raster", c("raster"))

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
    ee_read_raster(img_dsn$dsn, band_names, img_dsn$metadata)
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
