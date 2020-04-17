#' Save an EE Image in their local system
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
#' is set in the \code{via} argument, \code{ee_image_to_local} will make an
#' REST call to retrieve all the known information about the object. The
#' advantage of use "getInfo" is a direct and faster download. However, there
#' is a limitation of 262144 pixels by request which makes it not recommendable
#' for large images. Instead of "getInfo", the options: "drive" and "gcs"
#' are suitable for large collections since they use an intermediate web store
#' service. Before to use any of this options, it is necessary previously
#' install the R packages
#' \href{cran.r-project.org/web/packages/googledrive/index.html}{googledrive}
#' and \href{cran.r-project.org/web/packages/googleCloudStorageR/index.html}{
#' googleCloudStorageR}. For getting more information about exporting data take
#' a look at the \href{developers.google.com/earth-engine/exporting}{Google
#' Earth Engine Guide - Export data}.
#' @return A character object
#' @importFrom jsonlite parse_json
#' @importFrom raster raster stack
#' @importFrom sf st_transform st_coordinates st_make_grid
#' @importFrom stars st_set_dimensions st_mosaic st_dimensions
#' st_get_dimension_values
#' @examples
#' \dontrun{
#' library(rgee)
#'
#' # Initialize a specific Earth Engine account and load
#' # either Google Drive or Google Cloud Storage credentials
#' ee_reattach()
#' ee_Initialize(
#'   email = "data.colec.fbf@gmail.com",
#'   drive = TRUE,
#'   gcs = TRUE
#' )
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
#' img_01 <- ee_image_to_local(
#'   image = img,
#'   region = geometry,
#'   via = "getInfo"
#' )
#'
#' ## drive - Option 02
#' img_02 <- ee_image_to_local(
#'   image = img,
#'   region = geometry,
#'   via = "drive"
#' )
#'
#' ## gcs - Option 03
#' img_03 <- ee_image_to_local(
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
#' \code{ee_image_as_stars} will create a temporary file.
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
#' @param quiet Logical. Suppress info message
#' @details
#' \code{ee_image_as_stars} supports the download of \code{ee$Image}
#' by three different options: "getInfo", "drive", and "gcs". When "getInfo"
#' is set in the \code{via} argument, \code{ee_image_as_stars} will make an
#' REST call to retrieve all the known information about the object. The
#' advantage of use "getInfo" is a direct and faster download. However, there
#' is a limitation of 262144 pixels by request which makes it not recommendable
#' for large images. Instead of "getInfo", the options: "drive" and "gcs"
#' are suitable for large collections since they use an intermediate web store
#' service. Before to use any of this options, it is necessary previously
#' install the R packages
#' \href{cran.r-project.org/web/packages/googledrive/index.html}{googledrive}
#' and \href{cran.r-project.org/web/packages/googleCloudStorageR/index.html}{
#' googleCloudStorageR}. For getting more information about exporting data take
#' a look at the \href{developers.google.com/earth-engine/exporting}{Google
#' Earth Engine Guide - Export data}.
#' @return A stars-proxy object
#' @export
#' @examples
#' \dontrun{
#' library(rgee)
#'
#' # Initialize a specific Earth Engine account and load
#' # either Google Drive or Google Cloud Storage credentials
#' ee_reattach()
#' ee_Initialize(
#'   email = "data.colec.fbf@gmail.com",
#'   drive = TRUE,
#'   gcs = TRUE
#' )
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
#' img_01 <- ee_image_as_stars(
#'   image = img,
#'   region = geometry,
#'   via = "getInfo"
#' )
#'
#' ## drive - Method 02
#' img_02 <- ee_image_as_stars(
#'   image = img,
#'   region = geometry,
#'   via = "drive"
#' )
#'
#' ## gcs - Method 03
#' img_03 <- ee_image_as_stars(
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
ee_image_as_stars <- function(image,
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
#' \code{ee_image_as_raster} will create a temporary file.
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
#' is a limitation of 262144 pixels by request which makes it not recommendable
#' for large images. Instead of "getInfo", the options: "drive" and "gcs"
#' are suitable for large collections since they use an intermediate web store
#' service. Before to use any of this options, it is necessary previously
#' install the R packages
#' \href{cran.r-project.org/web/packages/googledrive/index.html}{googledrive}
#' and \href{cran.r-project.org/web/packages/googleCloudStorageR/index.html}{
#' googleCloudStorageR}. For getting more information about exporting data take
#' a look at the \href{developers.google.com/earth-engine/exporting}{Google
#' Earth Engine Guide - Export data}.
#' @return A RasterStack object
#' @examples
#' \dontrun{
#' library(rgee)
#'
#' # Initialize a specific Earth Engine account and load
#' # either Google Drive or Google Cloud Storage credentials
#' ee_reattach()
#' ee_Initialize(
#'   email = "data.colec.fbf@gmail.com",
#'   drive = TRUE,
#'   gcs = TRUE
#' )
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
#' img_01 <- ee_image_as_raster(
#'   image = img,
#'   region = geometry,
#'   via = "getInfo"
#' )
#'
#' ## drive - Method 02
#' img_02 <- ee_image_as_raster(
#'   image = img,
#'   region = geometry,
#'   via = "drive"
#' )
#'
#' ## gcs - Method 03
#' img_03 <- ee_image_as_raster(
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

#' Convert a local image into an EE Image
#'
#' @param x Character, RasterLayer, RasterStack, RasterBrick, stars or
#' stars-proxy object to be converted into an ee$Image.
#' @param assetId Character. Destination asset ID for the uploaded file.
#' @param bucket Character. Name of the GCS bucket.
#' @param monitoring Logical. If TRUE the exportation task will be monitored.
#' @param quiet Logical. Suppress info message.
#' @importFrom sf st_read st_sf st_sfc st_is_longlat
#' @importFrom geojsonio geojson_json
#' @return An ee$Image object
#' @name local_as_image
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
#' ee_gcs_to_image(
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
#' ee_stars_02 <- stars_as_ee(
#'   x = x,
#'   assetId = asset_id,
#'   bucket = "rgee_dev"
#' )
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

  ee_gcs_to_image(
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

#' @name local_as_image
#' @export
raster_as_ee <- stars_as_ee

#' @name local_as_image
#' @export
ee_local_image_as_ee <- stars_as_ee

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
  region_crs <- st_crs(sf_region)$epsg

  # region crs and image crs are equal?, otherwise force it.
  if (isFALSE(region_crs == img_crs)) {
    sf_region <- st_transform(sf_region, img_crs)
  }

  # Getting image ID if it is exist
  image_id <- tryCatch(
    expr = parse_json(image$id()$serialize())$
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

  ### Metadata getting from region and image
  # Band names
  band_names <- image$bandNames()$getInfo()
  #is geodesic?
  is_geodesic <- region$geodesic()$getInfo()
  #is evenodd?
  query_params <- unlist(parse_json(region$serialize())$scope)
  is_evenodd <- as.logical(
    query_params[grepl("evenOdd", names(query_params))]
  )
  if (length(is_evenodd) == 0 | is.null(is_evenodd)) {
    is_evenodd <- TRUE
  }
  ### -----------

  if (via == "getInfo") {
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
      npoints <- nrow(st_coordinates(sf_region))
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
      st_bbox() %>%
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
    sf_region_gridded <- suppressMessages(st_make_grid(sf_region, n = nbatch))
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
        'WKT      :', st_as_text(sf_region), "\n",
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
        st_as_stars() %>%
        `names<-`(image_id) %>%
        st_set_dimensions(names = c("x", "y", "band"))
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
      image_stars <- st_set_dimensions(image_stars, 3, values = band_names)
      stars_img_list[[r_index]] <- image_stars
    }

    # Analizing the stars dimensions
    dim_x <- st_get_dimension_values(stars_img_list[[1]],"x")
    dim_y <- st_get_dimension_values(stars_img_list[[1]],"y")

    if (length(dim_x) == 0 | length(dim_y) == 0) {
      stop(
        "The number of pixels of the resulting image in x (y) is zero. ",
        "Are you define the scale properly?"
      )
    } else {
      # Upgrading metadata of mosaic
      mosaic <- do.call(st_mosaic, stars_img_list)
      st_crs(mosaic) <- img_crs

      # The stars object have bands dimensions?
      if (!is.null(st_get_dimension_values(mosaic,"bands"))) {
        st_set_dimensions(mosaic, 3, values = band_names)
      }
      # Save results in dsn
      write_stars(mosaic, dsn)
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
        'WKT      :', st_as_text(sf_region), "\n",
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

    try(ee_monitoring(task = img_task, quiet = quiet))

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

    try(ee_monitoring(task = img_task, quiet = quiet))

    # From Google Cloud Storage to local
    cat('Moving image from GCS to Local ... Please wait  \n')
    dsn <- ee_gcs_to_local(img_task,  dsn = dsn, quiet = quiet)
  } else {
    stop("via argument invalid")
  }
  return(list(file = dsn, band_names = band_names, crs = img_crs))
}

