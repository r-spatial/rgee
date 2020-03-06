#' Convert an Earth Engine (EE) image in a stars object
#' @param image ee$Image to be converted into a sf object
#' @param region ee$Geometry$Polygon. Region of interest
#' @param scale The resolution in meters per pixel. If scale
#' is NULL, the native resolution of the first band will be
#' taken.
#' @param via Method to download the image. Three methods
#' are implemented 'getInfo', 'drive' and 'gcs'. See details.
#' @param container Relevant when the "via" argument is
#' defined as 'drive' or 'gcs'. The name of a unique
#' folder ('drive') or bucket ('gcs') to export into.
#' @param quiet logical. Suppress info message
#' @importFrom jsonlite parse_json
#' @importFrom sf st_transform st_coordinates
#' @importFrom stars st_set_dimensions
#' @details
#' The process to pass a ee$Image to your local env could be carried
#' out by three different strategies. The first one ('getInfo') use the getInfo
#' image method, which fetch and return information about Earth Engine
#' images, the advantage of use this is a direct and fast download.
#' However, there is a limit of 262144 pixels (512x512) that can
#' be transferred by request which makes it unsatisfactory for large images.
#' The second ('drive') and third ('gcs') method are suitable for large images
#' since it use Google Drive and Google Cloud Storage as a intermidiate
#' container.
#' @return A stars object, when via argument was defined as 'getInfo'. For
#' other cases will return a stars-proxy object.
#' @examples
#' library(raster)
#' library(stars)
#' library(rgee)
#' # Initialize a specific Earth Engine account and
#' # load Google Drive and Google Cloud Storage credentials
#' ee_reattach()
#' ee_Initialize(
#'   email = "data.colec.fbf@gmail.com",
#'   drive = TRUE,
#'   gcs = TRUE
#' )
#'
#' # Define an image.
#' img <- ee$Image('LANDSAT/LC08/C01/T1_SR/LC08_038029_20180810')$
#'   select(c('B4', 'B5', 'B6'))
#' Map$centerObject(img)
#' Map$addLayer(img)
#'
#' # Define an area of interest.
#' geometry <- ee$Geometry$Rectangle(c(-110.8, 44.6, -110.6, 44.7))
#'
#' ## getInfo - Method 01
#' img_stars_01 <- ee_as_stars(image = img,
#'                             region = geometry,
#'                             via = 'getInfo')
#' ## drive - Method 02
#' img_stars_02 <- ee_as_stars(image = img,
#'                             region = geometry,
#'                             via = 'drive')
#' ## gcs - Method 03
#' img_stars_03 <- ee_as_stars(image = img,
#'                             region = geometry,
#'                             container = 'rgee_dev',
#'                             via = 'gcs')
#' @export
ee_as_stars <- function(image,
                        region,
                        scale = NULL,
                        via = "getInfo",
                        container = "rgee_backup",
                        quiet = FALSE) {
  # Creating name for temporal file in drive or gcs
  time_format <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
  ee_description <- paste0("ee_as_stars_task_", time_format)
  image_id <- tryCatch(
    expr = parse_json(image$id()$serialize())$
      scope[[1]][[2]][["arguments"]][["id"]],
    error = function(e) "noid_"
  )
  band_names <- image$bandNames()$getInfo()
  file_name <- paste0(image_id, time_format)

  # Load ee_Initialize() session
  ee_path <- path.expand("~/.config/earthengine")
  ee_user <- read.table(
    file = sprintf("%s/rgee_sessioninfo.txt", ee_path),
    header = TRUE,
    stringsAsFactors = FALSE
  )
  if (via == "getInfo") {
    img_proj <- image$projection()$getInfo()
    if (!is.null(scale)) {
      image <- image$reproject(img_proj$crs, NULL, scale)
      img_proj <- image$projection()$getInfo()
    }
    # Image metadata
    image_epsg <- gsub("EPSG:", "", img_proj$crs) %>% as.numeric
    if (is.null(scale)) {
      img_scale_x <- img_proj$transform[1][[1]]
      img_scale_y <- img_proj$transform[5][[1]]
    } else {
      if (!is.numeric(scale)) {
        stop('scale argument needs to be a numeric vector')
      }
      if (length(scale) == 1) {
        img_scale_x <- scale
        img_scale_y <- -scale
      } else {
        stop('scale argument needs to be a numeric vector',
             ' of the form: c(x_scale, y_scale)')
      }
    }
    # Passing from region from earth engine to sf
    if (any(class(region) %in% "ee.geometry.Geometry")) {
      sf_region <- ee_as_sf(region) %>% st_transform(image_epsg)
      npoints <- sf_region$geometry %>% st_coordinates %>% nrow
      if (npoints != 5) {
        stop('region needs to be a ee$Geometry$Rectangle.')
      }
    } else  {
      stop('region needs to be a ee$Geometry$Rectangle.')
    }
    # Extracts a rectangular region of pixels from an image
    # into a 2D array per (return a Feature)
    ee_image_array <- image$sampleRectangle(region = region)

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
      st_set_dimensions(names = c("x", "y", "bands"))
    attr_dim <- attr(image_stars, "dimensions")

    ## Set Geotransform and dimensions to local image
    coord_matrix <- st_coordinates(sf_region)[,c('X','Y')]
    init_offset <- ee_fix_offset(img_proj, coord_matrix)
    min_long <- init_offset[1]
    max_lat <- init_offset[2]
    attr_dim$x$offset <- min_long
    attr_dim$y$offset <- max_lat
    attr_dim$x$delta <- img_scale_x
    attr_dim$y$delta <- img_scale_y
    attr(image_stars, "dimensions") <- attr_dim
    st_crs(image_stars) <- image_epsg
    st_set_dimensions(image_stars, 3, values = band_names)
  } else if (via == "drive") {
    if (is.na(ee_user$drive_cre)) {
      stop('Google Drive credentials were not loaded.',
           ' Run ee_Initialize(email = "myemail", drive = TRUE)',
           ' to fixed')
    }
    # From Earth Engine to Google Drive
    img_task <- ee_image_to_drive(
      image = image,
      description = ee_description,
      scale = scale,
      folder = container,
      fileFormat = "GEO_TIFF",
      region = region,
      fileNamePrefix = file_name
    )
    if (!quiet) {
      cat("From Earth Engine to Google Drive\n",
          "Google user :",ee_user$email,"\n",
          "Folder name :",container,"\n",
          "File name   :", image_id)
    }
    img_task$start()
    ee_monitoring(img_task)
    # From Google Drive to local
    image_stars <- ee_drive_to_local(img_task)
    if (length(band_names) > 1) {
      st_set_dimensions(image_stars, 3, values = band_names)
    } else {
      image_stars <- st_set_dimensions(image_stars, "bands")
      attr(image_stars,'dimensions')$bands$to <- 1
      st_set_dimensions(image_stars, 3, values = band_names)
    }
  } else if (via == "gcs") {
    if (is.na(ee_user$gcs_cre)) {
      stop('Google Drive credentials were not loaded.',
           ' Run ee_Initialize(email = "myemail", gcs = TRUE)',
           ' to fixed')
    }
    # From Earth Engine to Google Cloud Storage
    img_task <- ee_image_to_gcs(
      image = image,
      description = ee_description,
      bucket = container,
      fileFormat = "GEO_TIFF",
      region = region,
      scale = scale,
      fileNamePrefix = file_name
    )
    cat("Moving results from Earth Engine to Google Cloud Storage: ", image_id)
    if (!quiet) {
      cat("From Earth Engine to Google Cloud Storage\n",
          "Google user :",ee_user$email,"\n",
          "Bucket name :",container,"\n",
          "File name   :", image_id)
    }
    img_task$start()
    ee_monitoring(img_task)
    # From Google Cloud Storage to local
    image_stars <- ee_gcs_to_local(img_task)
    if (length(band_names) > 1) {
      st_set_dimensions(image_stars, 3, values = band_names)
    } else {
      image_stars <- st_set_dimensions(image_stars, "bands")
      attr(image_stars,'dimensions')$bands$to <- 1
      st_set_dimensions(image_stars, 3, values = band_names)
    }
  } else {
    stop("via argument invalid")
  }
}

#' Convert an sf object to an Earth Engine Image
#' @param x ee$Image
#' @export
stars_as_ee <- function(x) {

}

#' Fix offset of stars object
#' @noRd
ee_fix_offset <- function(img_proj,rectangle_coord){
  x_scale <- img_proj$transform[1][[1]]
  x_offset <- img_proj$transform[3][[1]]
  y_scale <- img_proj$transform[5][[1]]
  y_offset <- img_proj$transform[6][[1]]
  # X offset fixed
  x_offset_sf <- min(rectangle_coord[,'X'])
  x_npixels <- floor(abs((x_offset_sf - x_offset)/x_scale))
  x_offset_sf_fixed <- x_offset + x_npixels*x_scale
  # Y offset fixed
  y_offset_sf <- max(rectangle_coord[,'Y'])
  y_npixels <- floor(abs((y_offset_sf - y_offset)/y_scale))
  y_offset_sf_fixed <- y_offset + y_npixels*y_scale
  c(x_offset_sf_fixed, y_offset_sf_fixed)
}
