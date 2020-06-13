#' Convert an Earth Engine (EE) image into a stars object
#'
#' Convert an ee$Image into a stars object
#'
#' @param image ee$Image to be converted into a stars object
#' @param region EE Geometry Rectangle (ee$Geometry$Rectangle). The
#' CRS needs to be the same that the x argument otherwise it will be
#' forced. If not specified, image bounds will be taken.
#' @param dsn Character. Output filename. If missing,
#' \code{ee_as_stars} will create a temporary file.
#' @param scale Numeric. The resolution in meters per pixel. Defaults
#' to the native resolution of the image asset.
#' @param maxPixels Numeric. The maximum allowed number of pixels in the
#' exported image. The task will fail if the exported region covers
#' more pixels in the specified projection. Defaults to 100,000,000.
#' @param via Character. Method to fetch data about the object. Multiple
#' options supported. See details.
#' @param container Character. Name of the folder ('drive') or bucket ('gcs')
#' to be exported into (ignored if \code{via} is not defined as "drive" or
#' "gcs").
#' @param quiet Logical. Suppress info message
#'
#' @details
#' \code{ee_as_stars} supports the download of \code{ee$Image}
#' by three different options: "getInfo", "drive", and "gcs". When "getInfo"
#' is set in the \code{via} argument, \code{ee_as_stars} will make a
#' REST call to retrieve all the known information about the object. The
#' advantage of using "getInfo" is performing a quick download. However, there
#' is a limitation of 262144 pixels by request which makes it not recommendable
#' for large images. Instead of "getInfo", the options: "drive" and "gcs"
#' are suitable for large collections since they use an intermediate web store
#' service. Before using any of these options, it is necessary previously
#' to install the R packages
#' \href{ https://CRAN.R-project.org/package=googledrive}{googledrive}
#' and \href{https://CRAN.R-project.org/package=googleCloudStorageR}{
#' googleCloudStorageR}. For getting more information about exporting data take
#' a look at the \href{https://developers.google.com/earth-engine/exporting}{Google
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
#' ## getInfo - Option 01
#' img_01 <- ee_as_stars(
#'   image = img,
#'   region = geometry,
#'   via = "getInfo"
#' )
#'
#' ## drive - Method 02
#' img_02 <- ee_as_stars(
#'   image = img,
#'   region = geometry,
#'   via = "drive"
#' )
#'
#' ## gcs - Method 03
#' img_03 <- ee_as_stars(
#'   image = img,
#'   region = geometry,
#'   container = "rgee_dev",
#'   via = "gcs"
#' )
#'
#' # OPTIONAL: Delete containers
#' ee_clean_container(
#'   name = "rgee_backup",
#'   type = "drive"
#' )
#' ee_clean_container(
#'   name = "rgee_dev",
#'   type = "gcs"
#' )
#' }
#' @export
ee_as_stars <- function(image,
                              region,
                              dsn = NULL,
                              via = "getInfo",
                              scale = NULL,
                              maxPixels = 1e9,
                              container = "rgee_backup",
                              quiet = FALSE) {
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
    quiet = quiet
  )

  img_stars <- stars::read_stars(img_files$file,proxy = TRUE)
  sf::st_crs(img_stars) <- img_files$crs
  stars::st_set_dimensions(img_stars, 3, values = img_files$band_names)
}


#' Convert an Earth Engine (EE) image into a raster object
#'
#' Convert an ee$Image into a raster object
#'
#' @param image ee$Image to be converted into a raster object
#' @param region EE Geometry Rectangle (ee$Geometry$Rectangle). The
#' CRS needs to be the same that the x argument otherwise it will be
#' forced. If not specified, image bounds will be taken.
#' @param dsn Character. Output filename. If missing,
#' \code{ee_as_raster} will create a temporary file.
#' @param scale Numeric. The resolution in meters per pixel. Defaults
#' to the native resolution of the image asset.
#' @param maxPixels Numeric. The maximum allowed number of pixels in the
#' exported image. The task will fail if the exported region covers
#' more pixels in the specified projection. Defaults to 100,000,000.
#' @param via Character. Method to fetch data about the object. Multiple
#' options supported. See details.
#' @param container Character. Name of the folder ('drive') or bucket ('gcs')
#' to be exported into (ignored if \code{via} is not defined as "drive" or
#' "gcs").
#' @param quiet Logical. Suppress info message
#' @details
#' \code{ee_as_raster} supports the download of \code{ee$Image}
#' by three different options: "getInfo", "drive", and "gcs". When "getInfo"
#' is set in the \code{via} argument, \code{ee_as_stars} will make a
#' REST call to retrieve all the known information about the object. The
#' advantage of using "getInfo" is performing a quick download. However, there
#' is a limitation of 262144 pixels by request which makes it not recommendable
#' for large images. Instead of "getInfo", the options: "drive" and "gcs"
#' are suitable for large collections since they use an intermediate web store
#' service. Before using any of these options, it is necessary previously
#' to install the R packages
#' \href{ https://CRAN.R-project.org/package=googledrive}{googledrive}
#' and \href{https://CRAN.R-project.org/package=googleCloudStorageR}{
#' googleCloudStorageR}. For getting more information about exporting data take
#' a look at the \href{https://developers.google.com/earth-engine/exporting}{Google
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
#' ## getInfo - Option 01
#' img_01 <- ee_as_raster(
#'   image = img,
#'   region = geometry,
#'   via = "getInfo"
#' )
#'
#' ## drive - Method 02
#' img_02 <- ee_as_raster(
#'   image = img,
#'   region = geometry,
#'   via = "drive"
#' )
#'
#' ## gcs - Method 03
#' img_03 <- ee_as_raster(
#'   image = img,
#'   region = geometry,
#'   container = "rgee_dev",
#'   via = "gcs"
#' )
#'
#' # OPTIONAL: Delete containers
#' ee_clean_container(
#'   name = "rgee_backup",
#'   type = "drive"
#' )
#' ee_clean_container(
#'   name = "rgee_dev",
#'   type = "gcs"
#' )
#' }
#' @export
ee_as_raster  <- function(image,
                                region,
                                dsn = NULL,
                                via = "getInfo",
                                scale = NULL,
                                maxPixels = 1e9,
                                container = "rgee_backup",
                                quiet = FALSE) {
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
    quiet = quiet
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

#' Convert a stars or stars-proxy object into an EE Image
#'
#' Convert a stars or stars-proxy object into an ee$Image.
#'
#' @param x stars or stars-proxy object to be converted into an ee$Image.
#' @param assetId Character. Destination asset ID for the uploaded file.
#' @param overwrite Logical. If TRUE, the assetId will be overwritten.
#' @param bucket Character. Name of the GCS bucket.
#' @param monitoring Logical. If TRUE the exportation task will be monitored.
#' @param quiet Logical. Suppress info message.
#'
#' @return An ee$Image object
#' @family image upload functions
#' @examples
#' \dontrun{
#' library(rgee)
#' library(stars)
#' ee_Initialize(gcs = TRUE)
#'
#' # Get the filename of a image
#' tif <- system.file("tif/L7_ETMs.tif", package = "stars")
#' x <- read_stars(tif)
#' assetId <- sprintf("%s/%s",ee_get_assethome(),'stars_l7')
#'
#' # Method 1
#' # 1. Move from local to gcs
#' gs_uri <- local_to_gcs(x = tif, bucket = 'rgee_dev')
#'
#' # 2. Pass from gcs to asset
#' gcs_to_ee_image(
#'   x = x,
#'   gs_uri = gs_uri,
#'   assetId = assetId
#' )
#'
#' # OPTIONAL: Monitoring progress
#' ee_monitoring()
#'
#' # OPTIONAL: Display results
#' ee_stars_01 <- ee$Image(assetId)
#' Map$centerObject(ee_stars_01)
#' Map$addLayer(ee_stars_01)
#'
#' # Method 2
#' ee_stars_02 <- stars_as_ee(
#'   x = x,
#'   assetId = assetId,
#'   bucket = "rgee_dev"
#' )
#' Map$centerObject(ee_stars_02)
#' Map$addLayer(ee_stars_02)
#' }
#' @export
stars_as_ee <- function(x,
                        assetId,
                        overwrite = FALSE,
                        monitoring = TRUE,
                        bucket = NULL,
                        quiet = FALSE) {
  # Create a temporary shapefile as
  ee_temp <- tempdir()

  stars_proxy <- ee_as_proxystars(x, temp_dir = ee_temp)
  gcs_filename <- local_to_gcs(
    x = stars_proxy[[1]],
    bucket = bucket,
    quiet = quiet
  )

  gcs_to_ee_image(
    x = x,
    gs_uri = gcs_filename,
    overwrite = overwrite,
    assetId = assetId
  )

  if (isTRUE(monitoring)) {
    ee_monitoring()
    ee$Image(assetId)
  } else {
    assetId
  }
}


#' Convert a Raster* object into an EE Image
#'
#' Convert a Raster* object into an ee$Image.
#'
#' @param x RasterLayer, RasterStack or RasterBrick object to be converted into
#' an ee$Image.
#' @param assetId Character. Destination asset ID for the uploaded file.
#' @param overwrite Logical. If TRUE, the assetId will be overwritten.
#' @param bucket Character. Name of the GCS bucket.
#' @param monitoring Logical. If TRUE the exportation task will be monitored.
#' @param quiet Logical. Suppress info message.
#'
#' @return An ee$Image object
#' @family image upload functions
#'
#' @examples
#' \dontrun{
#' library(rgee)
#' library(stars)
#' ee_Initialize(gcs = TRUE)
#'
#' # Get the filename of a image
#' tif <- system.file("tif/L7_ETMs.tif", package = "stars")
#' x <- read_stars(tif)
#' assetId <- sprintf("%s/%s",ee_get_assethome(),'stars_l7')
#'
#' # Method 1
#' # 1. Move from local to gcs
#' gs_uri <- local_to_gcs(x = tif, bucket = 'rgee_dev')
#'
#' # 2. Pass from gcs to asset
#' gcs_to_ee_image(
#'   x = x,
#'   gs_uri = gs_uri,
#'   assetId = assetId
#' )
#'
#' # OPTIONAL: Monitoring progress
#' ee_monitoring()
#'
#' # OPTIONAL: Display results
#' ee_raster_01 <- ee$Image(assetId)
#' Map$centerObject(ee_raster_01)
#' Map$addLayer(ee_raster_01)
#'
#' # Method 2
#' ee_raster_02 <- raster_as_ee(
#'   x = x,
#'   assetId = assetId,
#'   bucket = "rgee_dev"
#' )
#' Map$centerObject(ee_raster_02)
#' Map$addLayer(ee_raster_02)
#' }
#' @export
raster_as_ee <- stars_as_ee

#' Passing an Earth Engine Image to Local
#' @noRd
ee_image_local <- function(image,
                           region,
                           dsn = NULL,
                           via = "getInfo",
                           scale = NULL,
                           maxPixels = 1e9,
                           container = "rgee_backup",
                           quiet = FALSE) {
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
  if (!any(class(region) %in% "ee.geometry.Geometry")) {
    stop("region argument is not an ee$geometry$Geometry")
  }

  # Default projection on an Image
  prj_image <- image$projection()$getInfo()
  img_crs <- as.numeric(gsub("EPSG:", "", prj_image$crs))

  # From geometry to sf
  sf_region <- ee_as_sf(x = region)$geometry
  region_crs <- sf::st_crs(sf_region)$epsg

  # region crs and image crs are equal?, otherwise force it.
  if (isFALSE(region_crs == img_crs)) {
    sf_region <- sf::st_transform(sf_region, img_crs)
  }

  # Getting image ID if it is exist
  image_id <- tryCatch(
    expr = jsonlite::parse_json(image$id()$serialize())$
      scope[[1]][[2]][["arguments"]][["id"]],
    error = function(e) "noid_image"
  )

  # Create description (Human-readable name of the task)
  # Relevant for either drive or gcs.
  time_format <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
  ee_description <- paste0("ee_as_stars_task_", time_format)
  file_name <- paste0(image_id, "_", time_format)

  # Have you loaded the necessary credentials?
  # Relevant for either drive or gcs.
  ee_user <- ee_exist_credentials()

  ### Metadata ---
  # Band names
  band_names <- image$bandNames()$getInfo()
  #is geodesic?
  is_geodesic <- region$geodesic()$getInfo()
  #is evenodd?
  query_params <- unlist(jsonlite::parse_json(region$serialize())$scope)
  is_evenodd <- as.logical(
    query_params[grepl("evenOdd", names(query_params))]
  )
  if (length(is_evenodd) == 0 | is.null(is_evenodd)) {
    is_evenodd <- TRUE
  }
  ### -----------

  if (via == "getInfo") {
    if (!requireNamespace("stars", quietly = TRUE)) {
      stop("package stars required, please install it first")
    }
    # initial crstransform
    xScale <- prj_image$transform[[1]]
    yScale <- -abs(prj_image$transform[[5]]) #always negative
    xshearing <- prj_image$transform[[2]]
    yshearing <- prj_image$transform[[4]]

    # getInfo does not support xShearing and yShearing different to zero
    if ((xshearing | yshearing) != 0) {
      stop(
        " 'getInfo' does not support xShearing and yShearing different",
        " to zero. Use 'drive' or 'gcs' instead."
      )
    }

    # region is a ee$Geometry$Rectangle?
    if (any(class(region) %in% "ee.geometry.Geometry")) {
      npoints <- nrow(sf::st_coordinates(sf_region))
      if (npoints != 5) {
        stop("region needs to be a ee$Geometry$Rectangle.")
      }
    }

    # Reproject image if you defined a scale
    # if not, get the scale from geotransform parameter
    if (!is.null(scale)) {
      if (!is.numeric(scale)) {
        stop("scale argument needs to be a numeric vector")
      }
      if (!length(scale) == 1) {
        stop("scale argument needs to be a one-length numeric vector")
      }
      nominalscale <- image$projection()$nominalScale()$getInfo()
      xScale <- xScale * scale/nominalscale
      yScale <- yScale * scale/nominalscale
      prj_image$transform[[1]] <- xScale
      prj_image$transform[[5]] <- yScale
      image <- image$reproject(
        crs = prj_image$crs,
        crsTransform = prj_image$transform
      )
    }

    # Estimating the number of pixels (approximately)
    # It is necessary just a single batch? (512x512)
    bbox <- sf_region %>%
      sf::st_bbox() %>%
      as.numeric()
    x_diff <- bbox[3] - bbox[1]
    y_diff <- bbox[4] - bbox[2]
    x_npixel <- tail(abs(x_diff / xScale))
    y_npixel <- tail(abs(y_diff / yScale))
    total_pixel <- x_npixel * y_npixel # approximately
    if (total_pixel > maxPixels) {
      stop(
        "Export too large. Specified ",
        total_pixel,
        " pixels (max:",
        maxPixels,
        "). ",
        "Specify higher maxPixels value if you",
        "intend to export a large area."
      )
    }

    # Warning message if your image is large
    maxPixels_getInfo <- 1024*1024
    nbatch <- ceiling(sqrt(total_pixel / (512 * 512)))
    if (nbatch > 3) {
      message(
        "Warning: getInfo is just for small images (max: ",
        maxPixels_getInfo,
        "). Use 'drive' or 'gcs' instead for faster download."
      )
    }

    # Create a regular tesselation over the bounding box
    # after that move to earth engine.
    sf_region_gridded <- suppressMessages(
      sf::st_make_grid(sf_region, n = nbatch)
    )
    region_fixed <- sf_region_gridded %>%
      sf_as_ee(
        check_ring_dir = TRUE,
        evenOdd = is_evenodd,
        proj = img_crs,
        geodesic = is_geodesic
      )

    # region parameters display
    if (!quiet) {
      cat(
        '- region parameters\n',
        'WKT      :', sf::st_as_text(sf_region), "\n",
        'CRS      :', img_crs, "\n",
        'geodesic :', is_geodesic, "\n",
        'evenOdd  :', is_evenodd, "\n"
      )
    }

    # ee$FeatureCollection to ee$List
    region_features <- region_fixed$toList(
      length(sf_region_gridded)
    )

    # Iterate for each tessellation
    stars_img_list <- list()
    if (!quiet) {
      if (nbatch * nbatch > 1) {
        cat(
          "region is too large ... creating ",
          length(sf_region_gridded), " patches.\n"
        )
      }
    }

    for (r_index in seq_len(nbatch * nbatch)) {
      if (!quiet) {
        if (nbatch * nbatch > 1) {
          cat(
            sprintf(
              "Getting data from the patch: %s/%s",
              r_index, nbatch * nbatch
            ), "\n"
          )
        }
      }
      index <- r_index - 1
      feature <- ee$Feature(region_features$get(index))$geometry()

      # Extracts a rectangular region of pixels from an image
      # into a 2D array per (return a Feature)
      ee_image_array <- image$sampleRectangle(
        region = feature,
        defaultValue = 0
      )

      # Extract pixel values band by band
      band_results <- list()
      for (index in seq_along(band_names)) {
        band <- band_names[index]
        band_results[[band]] <- ee_image_array$get(band)$getInfo()
        if (index == 1) {
          nrow_array <- length(band_results[[band]])
          ncol_array <- length(band_results[[band]][[1]])
        }
      }

      # Passing from an array to a stars object
      # Create array from a list
      image_array <- array(
        data = unlist(band_results),
        dim = c(ncol_array, nrow_array, length(band_names))
      )

      # Create stars object
      image_stars <- image_array %>%
        stars::st_as_stars() %>%
        `names<-`(image_id) %>%
        stars::st_set_dimensions(names = c("x", "y", "band"))
      attr_dim <- attr(image_stars, "dimensions")

      ## Configure metadata of the local image and geotransform
      sf_region_batch <- ee_as_sf(feature)

      # Fix the init_x and init_y of each image
      init_offset <- ee_fix_offset(
        img_transform = prj_image$transform,
        sf_region =  sf_region_batch
      )

      xTranslation_fixed <- init_offset[1]
      yTranslation_fixed <- init_offset[4]
      attr_dim$x$offset <- xTranslation_fixed
      attr_dim$y$offset <- yTranslation_fixed
      attr_dim$x$delta <- xScale
      attr_dim$y$delta <- yScale
      attr(image_stars, "dimensions") <- attr_dim
      image_stars <- stars::st_set_dimensions(
        .x = image_stars, which = 3, values = band_names
      )
      stars_img_list[[r_index]] <- image_stars
    }

    # Analizing the stars dimensions
    dim_x <- stars::st_get_dimension_values(stars_img_list[[1]],"x")
    dim_y <- stars::st_get_dimension_values(stars_img_list[[1]],"y")

    if (length(dim_x) == 1 | length(dim_y) == 1) {
      stop(
        "The number of pixels of the resulting image in x (y) is zero. ",
        "Are you define the scale properly?"
      )
    } else {
      # Upgrading metadata of mosaic
      mosaic <- do.call(stars::st_mosaic, stars_img_list)
      sf::st_crs(mosaic) <- img_crs

      # The stars object have bands dimensions?
      if (!is.null(stars::st_get_dimension_values(mosaic,"bands"))) {
        stars::st_set_dimensions(mosaic, 3, values = band_names)
      }

      # Save results in dsn
      stars::write_stars(mosaic, dsn)
    }
  } else if (via == "drive") {
    if (is.na(ee_user$drive_cre)) {
      ee_Initialize(email = ee_user$email, drive = TRUE)
      message(
        "Google Drive credentials were not loaded.",
        " Running ee_Initialize(email = '",ee_user$email,"', drive = TRUE)",
        " to fix it."
      )
    }

    # region parameter display
    if (!quiet) {
      cat(
        '- region parameters\n',
        'WKT      :', sf::st_as_text(sf_region), "\n",
        'CRS      :', img_crs, "\n",
        'geodesic :', is_geodesic, "\n",
        'evenOdd  :', is_evenodd, "\n"
      )
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
      fileNamePrefix = file_name
    )

    # download parameter display
    if (!quiet) {
      cat(
        "\n- download parameters (Google Drive)\n",
        "Image ID    :", image_id, "\n",
        "Google user :", ee_user$email, "\n",
        "Folder name :", container, "\n",
        "Date        :", time_format, "\n"
      )
    }
    img_task$start()

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
  } else if (via == "gcs") {
    if (is.na(ee_user$gcs_cre)) {
      ee_Initialize(email = ee_user$email, gcs = TRUE)
      message(
        "Google Cloud Storage credentials were not loaded.",
        " Running ee_Initialize(email = '",ee_user$email,"', gcs = TRUE)",
        " to fix it."
      )
    }

    # region parameter display
    if (!quiet) {
      cat(
        '- region parameters\n',
        'WKT      :', sf::st_as_text(sf_region), "\n",
        'CRS      :', img_crs, "\n",
        'geodesic :', is_geodesic, "\n",
        'evenOdd  :', is_evenodd, "\n"
      )
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
      fileNamePrefix = file_name
    )

    # download parameter display
    if (!quiet) {
      cat(
        "\n- download parameters (Google Cloud Storage)\n",
        "Image ID    :", image_id,
        "Google user :", ee_user$email, "\n",
        "Bucket name :", container, "\n",
        "Date        :", time_format, "\n"
      )
    }
    img_task$start()

    ee_monitoring(task = img_task, quiet = quiet)

    # From Google Cloud Storage to local
    cat('Moving image from GCS to Local ... Please wait  \n')
    dsn <- ee_gcs_to_local(img_task,  dsn = dsn, quiet = quiet)
  } else {
    stop("via argument invalid")
  }
  return(list(file = dsn, band_names = band_names, crs = img_crs))
}

#' Approximate size of an EE Image object
#'
#' Get the approximate number of rows, cols, and size of an
#' Earth Engine Image.
#'
#' @param image EE Image object.
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
#' l8 <- ee$Image("LANDSAT/LC08/C01/T1_SR/LC08_038029_20180810")
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
  if (band_length != 1) {
    stop("ee_image_info needs that image only has one band.")
  }
  img_proj <- image$projection()$getInfo()
  geotransform <- unlist(img_proj$transform)
  img_totalarea <- ee_as_sf(image$geometry(proj = img_proj$crs))
  suppressWarnings(
    sf::st_crs(img_totalarea) <- as.numeric(gsub("EPSG:", "", img_proj$crs))
  )
  bbox <- img_totalarea %>%
    sf::st_transform(as.numeric(gsub("EPSG:", "", img_proj$crs))) %>%
    sf::st_bbox() %>%
    as.numeric()

  x_diff <- bbox[3] - bbox[1]
  y_diff <- bbox[4] - bbox[2]
  x_npixel <- ceiling(abs(x_diff / geotransform[1]))
  y_npixel <- ceiling(abs(y_diff / geotransform[5]))
  total_pixel <- abs(as.numeric(x_npixel * y_npixel))

  if (!quiet) {
    cat('Image Rows       :', x_npixel,'\n')
    cat('Image Cols       :', y_npixel,'\n')
    cat('Number of Pixels :', format(total_pixel,scientific = FALSE),'\n')
  }

  if (isFALSE(getsize)) {
    invisible(
      list(
        nrow = x_npixel,
        ncol = y_npixel,
        total_pixel = total_pixel
      )
    )
  } else {
    image_id <- ee_utils_py_to_r(image$get("system:id")$getInfo())
    if (!is.null(image_id)) {
      image_size <- ee_manage_asset_size(image_id, quiet = TRUE) / band_length
    } else {
      bandtypes_info <- image$bandTypes()$getInfo()
      img_types <- unlist(bandtypes_info)
      band_types <- img_types[grepl("precision", names(img_types))]
      band_precision <- vapply(band_types, ee_get_typeimage_size, 0)
      number_of_bytes <- total_pixel * band_precision / compression_ratio
      image_size <- sum(number_of_bytes)
    }
    if (!quiet) {
      cat("Image Size       :", ee_humansize(image_size), "\n")
    }
    invisible(
      list(
        nrow = x_npixel,
        ncol = y_npixel,
        total_pixel = total_pixel,
        image_size = image_size
      )
    )
  }
}
