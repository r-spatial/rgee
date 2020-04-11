#' Save an Earth Engine (EE) image in their local system
#'
#' @param image ee$Image to be saved in the system.
#' @param region EE Geometry Rectangle (ee$Geometry$Rectangle). The
#' CRS needs to be the same that the x argument otherwise it will be
#' forced.
#' @param dsn Character. Output filename. If missing,
#' \code{ee_image_to_local} will create a temporary file.
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
#' \code{ee_image_to_local} supports the download of \code{ee$Image}
#' by three different options: "getInfo", "drive", and "gcs". When "getInfo"
#' is set in the \code{via} argument, \code{ee_image_as_character} will make an
#' REST call to retrieve all the known information about the object. The
#' advantage of use "getInfo" is a direct and faster download. However, there
#' is a limitation of 262144 pixels by request which makes it not recommendable
#' for large images. Instead of "getInfo", the options: "drive" and "gcs"
#' are suitable for large collections since they use an intermediate container.
#' They use Google Drive and Google Cloud Storage respectively. For getting
#' more information about exporting data take a look at the
#' \href{https://developers.google.com/earth-engine/exporting}{Google Earth
#' Engine Guide - Export data}.
#' @return A character object
#' @export
ee_image_to_local  <- function(image,
                               region,
                               dsn = NULL,
                               via = "getInfo",
                               scale = NULL,
                               maxPixels = 1e9,
                               container = "rgee_backup",
                               quiet = FALSE) {
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
  img_files$file
}

#' Convert an Earth Engine (EE) image into a stars object
#'
#' @param image ee$Image to be converted into a stars object
#' @param region EE Geometry Rectangle (ee$Geometry$Rectangle). The
#' CRS needs to be the same that the x argument otherwise it will be
#' forced. If not specified, image bounds will be taken.
#' @param dsn Character. Output filename. If missing,
#' \code{ee_image_as_character} will create a temporary file.
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
#' \code{ee_image_as_stars} supports the download of \code{ee$Image}
#' by three different options: "getInfo", "drive", and "gcs". When "getInfo"
#' is set in the \code{via} argument, \code{ee_image_as_stars} will make an
#' REST call to retrieve all the known information about the object. The
#' advantage of use "getInfo" is a direct and faster download. However, there
#' is a limitation of 5000 features by request which makes it not recommendable
#' for large collections. Instead of "getInfo", the options: "drive" and "gcs"
#' are suitable for large collections since they use an intermediate container.
#' They are Google Drive and Google Cloud Storage respectively. For getting
#' more information about exporting data take a look at the
#' \href{https://developers.google.com/earth-engine/exporting}{Google Earth
#' Engine Guide - Export data}.
#' @return A character object
#' @export
ee_image_as_stars  <- function(image,
                               region,
                               dsn = NULL,
                               via = "getInfo",
                               scale = NULL,
                               maxPixels = 1e9,
                               container = "rgee_backup",
                               quiet = FALSE) {
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
  img_stars <- read_stars(img_files$file,proxy = TRUE)
  st_crs(img_stars) <- img_files$crs
  st_set_dimensions(img_stars, 3, values = img_files$band_names)
}


#' Convert an Earth Engine (EE) image into a raster object
#'
#' @param image ee$Image to be converted into a raster object
#' @param region EE Geometry Rectangle (ee$Geometry$Rectangle). The
#' CRS needs to be the same that the x argument otherwise it will be
#' forced. If not specified, image bounds will be taken.
#' @param dsn Character. Output filename. If missing,
#' \code{ee_image_as_character} will create a temporary file.
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
#' \code{ee_image_as_raster} supports the download of \code{ee$Image}
#' by three different options: "getInfo", "drive", and "gcs". When "getInfo"
#' is set in the \code{via} argument, \code{ee_image_as_raster} will make an
#' REST call to retrieve all the known information about the object. The
#' advantage of use "getInfo" is a direct and faster download. However, there
#' is a limitation of 5000 features by request which makes it not recommendable
#' for large collections. Instead of "getInfo", the options: "drive" and "gcs"
#' are suitable for large collections since they use an intermediate container.
#' They are Google Drive and Google Cloud Storage respectively. For getting
#' more information about exporting data take a look at the
#' \href{https://developers.google.com/earth-engine/exporting}{Google Earth
#' Engine Guide - Export data}.
#' @return A character object
#' @export
ee_image_as_raster  <- function(image,
                                region,
                                dsn = NULL,
                                via = "getInfo",
                                scale = NULL,
                                maxPixels = 1e9,
                                container = "rgee_backup",
                                quiet = FALSE) {
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
    message("NOTE: To avoid memory excess problems, ee_image_as_raster will",
            " not build Raster objects for large images.")
    img_files$file
  } else {
    img_raster <- stack(img_files$file)
    names(img_raster) <- img_files$band_names
    img_raster
  }
}

#' Convert an stars object to an EE object
#'
#' @param x stars object to be converted into a ee$Image.
#' @param assetId Destination asset ID for the uploaded file.
#' @param bucket name you want this session to use by default,
#' or a bucket object.
#' @param monitoring If TRUE the exportation task will be monitored.
#' @param quiet Logical. Suppress info message.
#' @importFrom sf st_read st_sf st_sfc st_is_longlat
#' @importFrom geojsonio geojson_json
#' @return An ee$Image object
#' @details
#' The process to pass a sf object to Earth Engine Asset could be carried
#' out by three different strategies. These are controlled by the "via"
#' parameter. The first method implemented is 'getInfo'. In this method the
#' sf objects are transformed to GeoJSON using \link[geojsonio]{geojson_json}
#' and then encrusted in an HTTP request using the server-side objects that are
#' implemented in the Earth Engine API (e.g. ee$Geometry$*). If the sf object
#' is too large (>1Mb) it is likely to cause bottlenecks and plodding
#' connections. One advantage of this method is that it create temporary files
#' and will not be saved in your Earth Engine Asset. See
#' \href{https://developers.google.com/earth-engine/client_server}{Client
#' vs Server} documentation for more details. The second method implemented is
#' 'toasset'. It is similar to the previous one, with the difference that
#' the spatial object will be saved in your Earth Engine Asset. For dealing
#' with very large spatial objects, it is preferable to use the third method
#' called 'gcs'. In this method, firstly, the sf object will be saved as a
#' *.shp in the  /temp directory. Secondly, using the function ee_local_to_gcs
#' will move the shapefile from local to Google Cloud Storage. Finally, using
#' the function ee_gcs_to_asset_table it will be loaded to the Earth Engine
#' Asset.
#' @examples
#' \dontrun{
#' library(rgee)
#' library(stars)
#' ee_Initialize(gcs = TRUE)
#'
#' # Get the filename of a image
#' tif <- system.file("tif/L7_ETMs.tif", package = "stars")
#' x <- read_stars(tif)
#' asset_id <- sprintf("%s/%s",ee_get_assethome(),'stars_l7')
#'
#' # Method 1
#' # 1. Move from local to gcs
#' gs_uri <- ee_local_to_gcs(x = tif, bucket = 'rgee_dev')
#'
#' # 2. Pass from gcs to asset
#' ee_gcs_to_asset_image(
#'   x = x,
#'   gs_uri = gs_uri,
#'   asset_id = asset_id
#' )
#'
#' # OPTIONAL: Monitoring progress
#' ee_monitoring()
#'
#' # OPTIONAL: Display results
#' ee_stars_01 <- ee$Image(asset_id)
#' Map$centerObject(ee_stars_01)
#' Map$addLayer(ee_stars_01)
#'
#' # Method 2
#' ee_stars_02 <- stars_as_ee(x = x,
#'                            assetId = asset_id,
#'                            bucket = "rgee_dev")
#' Map$centerObject(ee_stars_02)
#' Map$addLayer(ee_stars_02)
#' }
#' @export
stars_as_ee <- function(x,
                        assetId,
                        monitoring = TRUE,
                        bucket = NULL,
                        quiet = FALSE) {
  # Create a temporary shapefile as
  ee_temp <- tempdir()

  stars_proxy <- ee_as_proxystars(x, temp_dir = ee_temp)
  gcs_filename <- ee_local_to_gcs(
    x = stars_proxy[[1]],
    bucket = bucket,
    quiet = quiet
  )
  ee_gcs_to_asset_image(
    x = x,
    gs_uri = gcs_filename,
    asset_id = assetId
  )
  if (isTRUE(monitoring)) {
    try(ee_monitoring())
    ee$Image(assetId)
  } else {
    assetId
  }
}



#' Passing an Earth Engine Image to Local
#' @importFrom jsonlite parse_json
#' @importFrom sf st_transform st_coordinates st_make_grid
#' @importFrom stars st_set_dimensions st_mosaic st_dimensions
#' @noRd
ee_image_local <- function(image,
                           region,
                           dsn = NULL,
                           via = "getInfo",
                           scale = NULL,
                           maxPixels = 1e9,
                           container = "rgee_backup",
                           quiet = FALSE) {
  if (is.null(dsn)) {
    dsn <- paste0(tempfile(),".tif")
  }

  if (!any(class(image) %in% "ee.image.Image")) {
    stop("x argument is not an ee$image$Image")
  }

  if (!any(class(region) %in% "ee.geometry.Geometry")) {
    stop("region argument is not an ee$geometry$Geometry")
  }

  prj_image <- image$projection()$getInfo()
  img_crs <- as.numeric(gsub("EPSG:", "", prj_image$crs))

  sf_region <- ee_table_as_sf(x = region)$geometry
  region_crs <- st_crs(sf_region)$epsg

  if (isFALSE(region_crs == img_crs)) {
    message(
      "The parameters region and x need to have the same crs",
      "\nEPSG region: ", region_crs,
      "\nEPSG x: ", img_crs,
      "\nForcing region to have the same CRS that x."
    )
    sf_region <- st_transform(sf_region, img_crs)
  }
  ## region is a ee$Geometry$Rectangle?
  if (any(class(region) %in% "ee.geometry.Geometry")) {
    npoints <- nrow(st_coordinates(sf_region))
    if (npoints != 5) {
      stop(
        stop("region needs to be a ee$Geometry$Rectangle.")
      )
    }
  }

  # Getting image ID if it is exist
  image_id <- tryCatch(
    expr = parse_json(image$id()$serialize())$
      scope[[1]][[2]][["arguments"]][["id"]],
    error = function(e) "noid_image"
  )

  # Creating name for temporal file; just for either drive or gcs
  time_format <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
  ee_description <- paste0("ee_as_stars_task_", time_format)
  file_name <- paste0(image_id, "_", time_format)

  # Load ee_Initialize() session; just for either drive or gcs
  ee_user <- rgee:::ee_exist_credentials()
  # Band names
  band_names <- image$bandNames()$getInfo()

  #is geodesic?
  is_geodesic <- region$geodesic()$getInfo()
  # is_evenodd?
  query_params <- unlist(parse_json(region$serialize())$scope)
  is_evenodd <- as.logical(
    query_params[grepl("evenOdd", names(query_params))]
  )
  if (length(is_evenodd) == 0 | is.null(is_evenodd)) {
    is_evenodd <- TRUE
  }

  if (via == "getInfo") {
    # fetch the scale
    if (isTRUE(is.null(scale))) {
      img_scale_x <- prj_image$transform[1][[1]]
      img_scale_y <- prj_image$transform[5][[1]]
    } else {
      if (!is.numeric(scale)) {
        stop("scale argument needs to be a numeric vector")
      }
      if (length(scale) == 1) {
        img_scale_x <- scale
        img_scale_y <- -scale
      } else if(length(scale) == 2) {
        img_scale_x <- scale[1]
        img_scale_y <- -scale[2]
      } else {
        stop(
          "scale argument needs to be a numeric vector",
          " of the form: c(x_scale, -y_scale)"
        )
      }
    }
    # Image metadata
    maxPixels_getInfo <- 1024*1024
    # It is necessary just single batch? (512x512)
    bbox <- sf_region %>%
      st_bbox() %>%
      as.numeric()
    x_diff <- bbox[3] - bbox[1]
    y_diff <- bbox[4] - bbox[2]
    x_npixel <- ceiling(abs(x_diff / prj_image$transform[1][[1]]))
    y_npixel <- ceiling(abs(y_diff / prj_image$transform[5][[1]]))
    total_pixel <- x_npixel * y_npixel
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
    nbatch <- ceiling(sqrt(total_pixel / (512 * 512)))
    if (nbatch > 3) {
      message(
        "Warning: getInfo is just for small images (max: ",
        maxPixels_getInfo,
        "). Use 'drive' or 'gcs' instead for faster download."
      )
    }
    sf_region_gridded <- st_make_grid(sf_region, n = nbatch)
    region_fixed <- sf_region_gridded %>%
      sf_as_ee(
        check_ring_dir = TRUE,
        evenOdd = is_evenodd,
        proj = img_crs,
        geodesic = is_geodesic
      )
    if (!quiet) {
      cat(
        '- region parameters\n',
        'WKT      :', st_as_text(sf_region), "\n",
        'CRS      :', img_crs, "\n",
        'geodesic :', is_geodesic, "\n",
        'evenOdd  :', is_evenodd, "\n"
      )
    }
    # FeatureCollection as a List
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
      ## Create array from a list
      image_array <- array(
        data = unlist(band_results),
        dim = c(ncol_array, nrow_array, length(band_names))
      )

      ## Create stars object
      image_stars <- image_array %>%
        st_as_stars() %>%
        `names<-`(image_id) %>%
        st_set_dimensions(names = c("x", "y", "band"))
      attr_dim <- attr(image_stars, "dimensions")

      ## Set Geotransform and dimensions to local image
      sf_region_batch <- ee_table_as_sf(feature)
      init_offset <- rgee:::ee_fix_offset(image, sf_region_batch)
      min_long <- init_offset[1]
      max_lat <- init_offset[2]
      attr_dim$x$offset <- min_long
      attr_dim$y$offset <- max_lat
      attr_dim$x$delta <- img_scale_x
      attr_dim$y$delta <- img_scale_y
      attr(image_stars, "dimensions") <- attr_dim
      image_stars <- st_set_dimensions(image_stars, 3, values = band_names)
      stars_img_list[[r_index]] <- image_stars
    }

    # Upgrading metadata of mosaic
    mosaic <- do.call(st_mosaic, stars_img_list)
    st_crs(mosaic) <- img_crs
    mosaic <- st_set_dimensions(mosaic, 3, values = band_names)
    # Save results in dsn
    write_stars(mosaic, dsn)
  } else if (via == "drive") {
    if (is.na(ee_user$drive_cre)) {
      stop(
        "Google Drive credentials were not loaded.",
        ' Run ee_Initialize(email = "myemail", drive = TRUE)',
        " to fix it"
      )
    }

    # Fixing geometry if it is necessary
    init_offset <- ee_fix_offset(image, sf_region)
    region_fixed <- sf_as_ee(
      x = sf_region,
      check_ring_dir = TRUE,
      evenOdd = is_evenodd,
      proj = img_crs,
      geodesic = is_geodesic
    )
    if (!quiet) {
      cat(
        '- region parameters\n',
        'WKT      :', st_as_text(sf_region), "\n",
        'CRS      :', img_crs, "\n",
        'geodesic :', is_geodesic, "\n",
        'evenOdd  :', is_evenodd, "\n"
      )
    }
    img_task <- ee_image_to_drive(
      image = image,
      description = ee_description,
      scale = scale,
      folder = container,
      fileFormat = "GEO_TIFF",
      region = region_fixed$geometry(),
      maxPixels = maxPixels,
      fileNamePrefix = file_name
    )
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

    try(ee_monitoring(task = img_task, quiet = quiet))

    # From Google Drive to local
    cat('Moving image from Google Drive to Local ... Please wait  \n')
    dsn <- ee_drive_to_local(task = img_task, dsn = dsn, consider = 'all')
  } else if (via == "gcs") {
    if (is.na(ee_user$gcs_cre)) {
      stop(
        "Google Cloud Storage credentials were not loaded.",
        ' Run ee_Initialize(email = "myemail", gcs = TRUE)',
        " to fix it"
      )
    }
    # Fixing geometry if it is necessary
    init_offset <- ee_fix_offset(image, sf_region)
    region_fixed <- sf_as_ee(
      x = sf_region,
      check_ring_dir = TRUE,
      evenOdd = is_evenodd,
      proj = img_crs,
      geodesic = is_geodesic
    )

    if (!quiet) {
      cat(
        '- region parameters\n',
        'WKT      :', st_as_text(sf_region), "\n",
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
      region = region_fixed$geometry(),
      maxPixels = maxPixels,
      scale = scale,
      fileNamePrefix = file_name
    )

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
    try(ee_monitoring(task = img_task, quiet = quiet))

    # From Google Cloud Storage to local
    cat('Moving image from GCS to Local ... Please wait  \n')
    dsn <- ee_gcs_to_local(img_task,  dsn = dsn)
  } else {
    stop("via argument invalid")
  }
  return(list(file = dsn, band_names = band_names, crs = img_crs))
}
