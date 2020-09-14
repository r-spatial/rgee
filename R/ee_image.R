#' Convert an Earth Engine (EE) image into a stars object
#'
#' Convert an ee$Image into a stars object
#'
#' @param image ee$Image to be converted into a stars object
#' @param region EE Geometry (ee$Geometry$Polygon) which specify the region
#' to export. CRS needs to be the same that the x argument otherwise it will be
#' forced. If not specified image bounds will be taken.
#' @param dsn Character. Output filename. If missing,
#' \code{ee_as_stars} will create a temporary file.
#' @param scale Numeric. The resolution in meters per pixel. Defaults
#' to the native resolution of the image asset.
#' @param maxPixels Numeric. The maximum allowed number of pixels in the
#' exported image. The task will fail if the exported region covers
#' more pixels in the specified projection. Defaults to 100,000,000.
#' @param via Character. Method to fetch data about the object. Two methods
#' are implemented: "drive", "gcs". See details.
#' @param container Character. Name of the folder ('drive') or bucket ('gcs')
#' to be exported into (ignored if \code{via} is not defined as "drive" or
#' "gcs").
#' @param quiet Logical. Suppress info message
#' @param ... Extra exporting argument. See \link{ee_image_to_drive} and
#' \link{ee_image_to_gcs}.
#'
#' @details
#' \code{ee_as_stars} supports the download of \code{ee$Image}
#' by two different options: "drive" that use Google Drive and "gcs"
#' that use Google Cloud Storage. Previously, it is necessary to install the
#' R packages \href{ https://CRAN.R-project.org/package=googledrive}{googledrive}
#' or \href{https://CRAN.R-project.org/package=googleCloudStorageR}{
#' googleCloudStorageR} respectively. For getting more information about
#' exporting data from Earth Engine,  take a look at the
#' \href{https://developers.google.com/earth-engine/exporting}{Google
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
#' ## drive - Method 01
#' img_02 <- ee_as_stars(
#'   image = img,
#'   region = geometry,
#'   via = "drive"
#' )
#'
#' ## gcs - Method 02
#' # img_03 <- ee_as_stars(
#' #   image = img,
#' #  region = geometry,
#' #   container = "rgee_dev",
#' #  via = "gcs"
#' #)
#'
#' # OPTIONAL: Delete containers
#' # ee_clean_container(name = "rgee_backup", type = "drive")
#' # ee_clean_container(name = "rgee_dev", type = "gcs")
#' }
#' @export
ee_as_stars <- function(image,
                        region = NULL,
                        dsn = NULL,
                        via = "drive",
                        scale = NULL,
                        maxPixels = 1e9,
                        container = "rgee_backup",
                        quiet = FALSE,
                        ...) {
  if (!requireNamespace("stars", quietly = TRUE)) {
    stop("package stars required, please install it first")
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("package sf required, please install it first")
  }

  img_files <- ee_image_local(
    image = image,
    region = region,
    dsn = dsn,
    via = via,
    scale = scale,
    maxPixels = maxPixels,
    container = container,
    quiet = quiet,
    ...
  )

  img_stars <- stars::read_stars(img_files$file, proxy = TRUE)

  # It's a single image?
  if (length(stars::st_dimensions(img_stars)) < 3) {
    img_stars
  } else {
    stars::st_set_dimensions(img_stars, 3, values = img_files$band_names)
  }
}


#' Convert an Earth Engine (EE) image into a raster object
#'
#' Convert an ee$Image into a raster object
#'
#' @param image ee$Image to be converted into a raster object
#' @param region EE Geometry (ee$Geometry$Polygon) which specify the region
#' to export. CRS needs to be the same that the x argument otherwise it will be
#' forced. If not specified image bounds will be taken.
#' @param dsn Character. Output filename. If missing,
#' \code{ee_as_raster} will create a temporary file.
#' @param scale Numeric. The resolution in meters per pixel. Defaults
#' to the native resolution of the image asset.
#' @param maxPixels Numeric. The maximum allowed number of pixels in the
#' exported image. The task will fail if the exported region covers
#' more pixels in the specified projection. Defaults to 100,000,000.
#' @param via Character. Method to fetch data about the object. Two methods
#' are implemented: "drive", "gcs". See details.
#' @param container Character. Name of the folder ('drive') or bucket ('gcs')
#' to be exported into (ignored if \code{via} is not defined as "drive" or
#' "gcs").
#' @param quiet Logical. Suppress info message
#' @param ... Extra exporting argument. See \link{ee_image_to_drive} and
#' \link{ee_image_to_gcs}.
#' @details
#' \code{ee_as_raster} supports the download of \code{ee$Image}
#' by two different options: "drive" that use Google Drive and "gcs"
#' that use Google Cloud Storage. Previously, it is necessary to install the
#' R packages \href{ https://CRAN.R-project.org/package=googledrive}{googledrive}
#' or \href{https://CRAN.R-project.org/package=googleCloudStorageR}{
#' googleCloudStorageR} respectively. For getting more information about
#' exporting data from Earth Engine, take a look at the
#' \href{https://developers.google.com/earth-engine/exporting}{Google
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
#' img_02 <- ee_as_raster(
#'   image = img,
#'   region = geometry,
#'   via = "drive"
#' )
#'
#' ## gcs - Method 02
#' # img_03 <- ee_as_raster(
#' #   image = img,
#' #   region = geometry,
#' #   container = "rgee_dev",
#' #   via = "gcs"
#' # )
#'
#' # OPTIONAL: Delete containers
#' # ee_clean_container(name = "rgee_backup", type = "drive")
#' # ee_clean_container(name = "rgee_dev", type = "gcs")
#' }
#' @export
ee_as_raster  <- function(image,
                          region = NULL,
                          dsn = NULL,
                          via = "drive",
                          scale = NULL,
                          maxPixels = 1e9,
                          container = "rgee_backup",
                          quiet = FALSE,
                          ...) {
  if (!requireNamespace("raster", quietly = TRUE)) {
    stop("package raster required, please install it first")
  }
  img_files <- ee_image_local(
    image = image,
    region = region,
    dsn = dsn,
    via = via,
    scale = scale,
    maxPixels = maxPixels,
    container = container,
    quiet = quiet,
    ...
  )

  if (length(img_files$file) > 1) {
    message("NOTE: To avoid memory excess problems, ee_as_raster will",
            " not build Raster objects for large images.")
    img_files$file
  } else {
    img_raster <- raster::stack(img_files$file)
    names(img_raster) <- img_files$band_names
    img_raster
  }
}

#' Passing an Earth Engine Image to Local
#' @noRd
ee_image_local <- function(image,
                           region,
                           dsn = NULL,
                           via = "drive",
                           scale = NULL,
                           maxPixels = 1e9,
                           container = "rgee_backup",
                           quiet = FALSE,
                           ...) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("package sf required, please install it first")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("package jsonlite required, please install it first")
  }
  if (!requireNamespace("stars", quietly = TRUE)) {
    stop("package stars required, please install it first")
  }

  # if dsn is NULL, dsn will be a /tempfile.
  if (is.null(dsn)) {
    dsn <- paste0(tempfile(),".tif")
  }

  # is image an ee.image.Image?
  if (!any(class(image) %in% "ee.image.Image")) {
    stop("x argument is not an ee$image$Image")
  }

  # is region an ee.geometry.Geometry?
  if (!any(class(region) %in% c("ee.geometry.Geometry", "NULL"))) {
    stop("region argument is not an ee$geometry$Geometry")
  }

  # Get bandnames
  band_names <- image$bandNames()$getInfo()

  if (via == "getInfo") {
    ee_image_local_getInfo(image, region, dsn, scale, maxPixels,
                           container, band_names, quiet)
  } else if (via == "drive") {
    ee_image_local_drive(image, region, dsn, scale, maxPixels,
                         container, quiet, ...)
  } else if (via == "gcs") {
    ee_image_local_gcs(image, region, dsn, scale, maxPixels,
                       container, quiet, ...)
  } else {
    stop("via argument invalid")
  }
  list(file = dsn, band_names = band_names)
}

#' Passing an Earth Engine Image to Local using drive
#' @noRd
ee_image_local_drive <- function(image, region, dsn, scale, maxPixels,
                                 container, quiet, ...) {

  # Have you loaded the necessary credentials?
  # Relevant for either drive or gcs.
  ee_user <- ee_exist_credentials()

  # Getting image ID if it is exist
  image_id <- tryCatch(
    expr = jsonlite::parse_json(image$id()$serialize())$
      scope[[1]][[2]][["arguments"]][["id"]],
    error = function(e) "noid_image"
  )
  if (is.null(image_id)) {
    image_id <- "noid_image"
  }

  # Create description (Human-readable name of the task)
  # Relevant for either drive or gcs.
  time_format <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
  ee_description <- paste0("ee_as_stars_task_", time_format)
  file_name <- paste0(image_id, "_", time_format)

  # Are GD credentials loaded?
  if (is.na(ee_user$drive_cre)) {
    ee_Initialize(email = ee_user$email, drive = TRUE)
    message(
      "Google Drive credentials were not loaded.",
      " Running ee_Initialize(email = '",ee_user$email,"', drive = TRUE)",
      " to fix it."
    )
  }

  # region parameter display
  if (!is.null(region)) {
    ee_geometry_message(region, quiet = quiet)
  }

  # From Google Earth Engine to Google Drive
  img_task <- ee_image_to_drive(
    image = image,
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
  ee_monitoring(task = img_task, quiet = quiet)

  # From Google Drive to local
  if (isFALSE(quiet)) {
    cat('Moving image from Google Drive to Local ... Please wait  \n')
  }

  dsn <- ee_drive_to_local(
    task = img_task,
    dsn = dsn,
    consider = 'all',
    quiet = quiet
  )
  invisible(dsn)
}

#' Passing an Earth Engine Image to Local using gcs
#' @noRd
ee_image_local_gcs <- function(image, region, dsn, scale, maxPixels,
                               container, quiet, ...) {
  # Have you loaded the necessary credentials?
  # Relevant for either drive or gcs.
  ee_user <- ee_exist_credentials()

  if (is.na(ee_user$gcs_cre)) {
    ee_Initialize(email = ee_user$email, gcs = TRUE)
    message(
      "Google Cloud Storage credentials were not loaded.",
      " Running ee_Initialize(email = '",ee_user$email,"', gcs = TRUE)",
      " to fix it."
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

  # From Earth Engine to Google Cloud Storage
  img_task <- ee_image_to_gcs(
    image = image,
    description = ee_description,
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
  ee_monitoring(task = img_task, quiet = quiet)

  # From Google Cloud Storage to local
  cat('Moving image from GCS to Local ... Please wait  \n')
  dsn <- ee_gcs_to_local(img_task,  dsn = dsn, quiet = quiet)
  invisible(dsn)
}

#' Approximate size of an EE Image object
#'
#' Get the approximate number of rows, cols, and size of an single-band
#' Earth Engine Image.
#'
#' @param image Single-band EE Image object.
#' @param getsize Logical. If TRUE, the size of the object
#' will be estimated.
#' @param compression_ratio Numeric. Measurement of the relative reduction
#' in size of data representation produced by a data compression algorithm
#' (ignored if \code{getsize} is FALSE). By default is 20
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
                          getsize = TRUE,
                          compression_ratio = 20,
                          quiet = FALSE) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("package sf required, please install it first")
  }
  band_length <- length(image$bandNames()$getInfo())

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
  img_proj_wkt <- ee_utils_get_crs(img_proj$crs)

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

#' Approx number of pixels
#' @noRd
ee_approx_number_pixels <- function(region, geotransform) {
  bbox <- region %>%
    sf::st_bbox() %>%
    as.numeric()

  # necessary info
  x_diff <- bbox[3] - bbox[1]
  y_diff <- bbox[4] - bbox[2]
  xScale <- geotransform$transform[[1]]
  yScale <- geotransform$transform[[5]]
  x_npixel <- tail(abs(x_diff / xScale))
  y_npixel <- tail(abs(y_diff / yScale))
  round(x_npixel * y_npixel) # approximately
}
