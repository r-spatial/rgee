#' Convert an Earth Engine (EE) image in a stars object
#' @param image ee$Image to be converted into a sf object
#' @param region EE Geometry Rectangle (ee$Geometry$Rectangle). The
#' CRS needs to be the same that the x argument otherwise it will be
#' forced. If it is not specified image bounds will be taken.
#' @param scale The resolution in meters per pixel. If scale
#' is NULL, the native resolution of the first band will be
#' taken.
#' @param geodesic Whether line segments of region should be interpreted as
#' spherical geodesics. If FALSE, indicates that line segments should be
#' interpreted as planar lines in the specified CRS. If it is not specified in
#' the geometry (region argument) defaults to TRUE if the CRS is geographic
#' (including the default EPSG:4326), or to FALSE if the CRS is projected.
#' @param evenOdd If TRUE, polygon interiors will be determined by
#' the even/odd rule, where a point is inside if it crosses an odd
#' number of edges to reach a point at infinity. Otherwise polygons
#' use the left- inside rule, where interiors are on the left side
#' of the shell's edges when walking the vertices in the given order.
#' If unspecified in the geometry (region argument) defaults to FALSE.
#' @param maxPixels The maximum allowed number of pixels in the
#' exported image. The task will fail if the exported region covers
#' more pixels in the specified projection. Defaults to 100,000,000.
#' @param via Method to download the image. Three methods
#' are implemented 'getInfo', 'drive' and 'gcs'. See details.
#' @param container Relevant when the "via" argument is
#' defined as 'drive' or 'gcs'. It is the name of a unique
#' folder ('drive') or bucket ('gcs') to export into.
#' @param monitoring Relevant when the "via" argument is
#' defined as 'drive' or 'gcs'.If it is FALSE, ee_as_stars will wait
#' until the task is finished.
#' @param quiet logical. Suppress info message
#' @importFrom jsonlite parse_json
#' @importFrom sf st_transform st_coordinates st_make_grid
#' @importFrom stars st_set_dimensions st_mosaic st_dimensions
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
                        geodesic = NULL,
                        evenOdd = NULL,
                        maxPixels = 1e9,
                        monitoring = TRUE,
                        via = "getInfo",
                        container = "rgee_backup",
                        quiet = FALSE) {
  if (!any(class(image) %in%  "ee.image.Image")) {
    stop("image argument is not an ee$image$Image")
  }
  region_generated <- FALSE
  if (missing(region)) {
    message('region is not defined ... taking the image bounds.')
    region <- image$geometry()
    region_generated <- TRUE
  }
  if (!any(class(region) %in% "ee.geometry.Geometry")) {
    stop("region argument is not an ee$geometry$Geometry")
  }

  # region testing
  prj_image <- image$projection()$getInfo()
  sf_image <- ee_as_sf(image$geometry())$geometry %>%
    st_transform(as.numeric(gsub('EPSG:','',prj_image$crs)))
  sf_region <- ee_as_sf(region)$geometry %>%
    st_transform(as.numeric(gsub('EPSG:','',prj_image$crs)))

  if (is.null(geodesic)) {
    if (region_generated) {
      is_geodesic <- st_is_longlat(sf_region)
    } else {
      is_geodesic <- region$geodesic()$getInfo()
    }
  } else {
    is_geodesic <- geodesic
  }

  if (is.null(evenOdd)) {
    query_params <- unlist(parse_json(region$serialize())$scope)
    is_evenodd <- as.logical(
      query_params[grepl("evenOdd", names(query_params))]
    )
    if (length(is_evenodd) == 0 | is.null(is_evenodd)) {
      is_evenodd <- FALSE
    }
  } else {
    is_evenodd <- evenOdd
  }

  if (!identical(st_crs(sf_image), st_crs(sf_region))) {
    stop('The parameters region and x need to have the same crs\n',
         'EPSG region: ', st_crs(sf_region)$epsg,
         '\nEPSG x: ', st_crs(sf_image)$epsg)
  }

  if (any(class(region) %in% "ee.geometry.Geometry")) {
    npoints <- nrow(st_coordinates(sf_region))
    if (npoints != 5) {
      message('region argument needs to be a ee$Geometry$Rectangle. ',
              'Fixing it running region$bounds() ...\n')
      region <- region$bounds()
      sf_region <- ee_as_sf(region)$geometry %>%
        st_transform(as.numeric(gsub('EPSG:','',prj_image$crs)))
    }
  } else  {
    stop('region needs to be a ee$Geometry$Rectangle.')
  }

  ## region is a world scene?
  if (any(st_bbox(sf_region) %in% c(180,-90))) {
    sf_region <- ee_fix_world_region(sf_image, sf_region, quiet)$geometry
  }

  # Getting image ID if it is exist
  image_id <- tryCatch(
    expr = parse_json(image$id()$serialize())$
      scope[[1]][[2]][["arguments"]][["id"]],
    error = function(e) "thumbnail"
  )

  # Creating name for temporal file; just for either drive or gcs
  time_format <- format(Sys.time(), "%Y-%m-%d-%H:%M:%S")
  ee_description <- paste0("ee_as_stars_task_", time_format)
  file_name <- paste0(image_id,'_', time_format)
  # Load ee_Initialize() session; just for either drive or gcs
  ee_path <- path.expand("~/.config/earthengine")
  ee_user <- read.table(
    file = sprintf("%s/rgee_sessioninfo.txt", ee_path),
    header = TRUE,
    stringsAsFactors = FALSE
  )

  # Band names
  band_names <- image$bandNames()$getInfo()

  if (via == "getInfo") {
    if (!is.null(scale)) {
      image <- image$reproject(prj_image$crs, NULL, scale)
      prj_image <- image$projection()$getInfo()
    }
    # Image metadata
    image_epsg <- as.numeric(gsub("EPSG:", "", prj_image$crs))
    if (is.null(scale)) {
      img_scale_x <- prj_image$transform[1][[1]]
      img_scale_y <- prj_image$transform[5][[1]]
    } else {
      if (!is.numeric(scale)) {
        stop('scale argument needs to be a numeric vector')
      }
      if (length(scale) == 1) {
        img_scale_x <- scale
        img_scale_y <- -scale
      } else {
        stop('scale argument needs to be a numeric vector',
             ' of the form: c(x_scale, -y_scale)')
      }
    }

    # It is necessary just single batch? (512x512)
    bbox <- sf_region %>%
      st_bbox() %>%
      as.numeric()
    x_diff <- bbox[3] - bbox[1]
    y_diff <- bbox[4] - bbox[2]
    x_npixel <- round(abs(x_diff/prj_image$transform[1][[1]]))
    y_npixel <- round(abs(y_diff/prj_image$transform[5][[1]]))
    total_pixel <- x_npixel*y_npixel
    if (total_pixel > maxPixels) {
      stop(
        sprintf('Export too large: specified %s pixels (max: %s).',
                total_pixel,format(maxPixels,scientific = FALSE)),
        'Specify higher maxPixels value if you intend to',
        'export a large area.')
    }

    nbatch <- ceiling(sqrt(total_pixel/(512*512)))
    if (nbatch > 3) {
      stop('define "getInfo" in via argument is just for small images',
           ' (< ~1536x1536). Please use "drive" or "gcs" instead.')
    }
    sf_region_gridded <- st_make_grid(sf_region,n = nbatch)
    ee_crs <- st_crs(sf_region)$epsg
    region_fixed <- sf_region_gridded %>%
      sf_as_ee(check_ring_dir = TRUE,
               evenOdd = is_evenodd,
               proj = ee_crs,
               geodesic = is_geodesic)
    if (!quiet) {
      cat(
        '- region parameters\n',
        '\rWKT      :', st_as_text(sf_region),
        '\nCRS      :', ee_crs,
        '\ngeodesic :', is_geodesic,
        '\nevenOdd  :', is_evenodd,
        '\n'
      )
    }
    # FeatureCollection as a List
    region_features <- region_fixed$toList(
      length(sf_region_gridded)
    )

    #Iterate for each tessellation
    stars_img_list <- list()
    if (!quiet) {
      if (nbatch*nbatch > 1) {
        cat('region is too large ... creating ',
            length(sf_region_gridded),' patches.\n')
      }
    }
    for (r_index in seq_len(nbatch*nbatch)) {
      if (!quiet) {
        if (nbatch*nbatch > 1) {
          cat(
            sprintf('Getting data for the patch: %s/%s',
                    r_index, nbatch*nbatch),'\n'
          )
        }
      }
      index <- r_index - 1
      feature <- ee$Feature(region_features$get(index))$geometry()
      # Extracts a rectangular region of pixels from an image
      # into a 2D array per (return a Feature)
      ee_image_array <- image$sampleRectangle(region = feature)
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
      sf_region_batch <- ee_as_sf(feature)
      init_offset <- ee_fix_offset(image, sf_region_batch)
      min_long <- init_offset[1]
      max_lat <- init_offset[2]
      attr_dim$x$offset <- min_long
      attr_dim$y$offset <- max_lat
      attr_dim$x$delta <- img_scale_x
      attr_dim$y$delta <- img_scale_y
      attr(image_stars, "dimensions") <- attr_dim
      st_crs(image_stars) <- image_epsg
      image_stars <- st_set_dimensions(image_stars, 3, values = band_names)
      stars_img_list[[r_index]] <- image_stars
    }
    mosaic <- do.call(st_mosaic, stars_img_list)
    if (is.null(st_dimensions(mosaic)$band)) {
      mosaic
    } else {
      st_set_dimensions(mosaic, 3, values = band_names)
    }
  } else if (via == "drive") {
    if (is.na(ee_user$drive_cre)) {
      stop('Google Drive credentials were not loaded.',
           ' Run ee_Initialize(email = "myemail", drive = TRUE)',
           ' to fixed')
    }

    # Fixing geometry if it is necessary
    init_offset <- ee_fix_offset(image, sf_region)
    ee_crs <- st_crs(sf_region)$epsg
    region_fixed <- sf_as_ee(x = sf_region,
                             check_ring_dir = TRUE,
                             evenOdd = is_evenodd,
                             proj = ee_crs,
                             geodesic = is_geodesic)
    if (!quiet) {
      cat(
        '- region parameters\n',
        '\rWKT      :', st_as_text(sf_region),
        '\nCRS      :', ee_crs,
        '\ngeodesic :', is_geodesic,
        '\nevenOdd  :', is_evenodd,
        '\n'
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
      cat("\n- download parameters (Google Drive)\n",
          "\rImage ID    :", image_id,
          "\rGoogle user :",ee_user$email,"\n",
          "\rFolder name :",container,"\n",
          "\rDate        :", time_format, "\n")
    }
    img_task$start()
    if (isTRUE(monitoring)) {
      ee_monitoring(img_task)
    }
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

    # Fixing geometry if it is necessary
    init_offset <- ee_fix_offset(image, sf_region)
    ee_crs <- st_crs(sf_region)$epsg
    region_fixed <- sf_as_ee(x = sf_region,
                             check_ring_dir = TRUE,
                             evenOdd = is_evenodd,
                             proj = ee_crs,
                             geodesic = is_geodesic)
    if (!quiet) {
      cat(
        '- region parameters\n',
        '\rWKT      :', st_as_text(sf_region),
        '\nCRS      :', ee_crs,
        '\ngeodesic :', is_geodesic,
        '\nevenOdd  :', is_evenodd,
        '\n'
      )
    }
    # From Earth Engine to Google Cloud Storage
    img_task <- ee_image_to_gcs(
      image = image,
      description = ee_description,
      bucket = container,
      fileFormat = "GEO_TIFF",
      region = region_fixed$geometry(),
      scale = scale,
      fileNamePrefix = file_name
    )
    if (!quiet) {
      cat("\n- download parameters (Google Cloud Storage)\n",
          "\rImage ID    :", image_id,
          "\rGoogle user :",ee_user$email,"\n",
          "\rFolder name :",container,"\n",
          "\rDate        :", time_format, "\n")

    }
    img_task$start()
    if (isTRUE(monitoring)) {
      ee_monitoring(img_task)
    }
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
ee_fix_offset <- function(image, sf_region) {
  img_proj <- image$projection()$getInfo()
  rectangle_coord <- st_coordinates(sf_region)
  # image spatial parameters
  img_x_scale <- img_proj$transform[1][[1]]
  img_x_offset <- img_proj$transform[3][[1]]
  img_y_scale <- img_proj$transform[5][[1]]
  img_y_offset <- img_proj$transform[6][[1]]

  # X offset fixed
  sf_x_min <- min(rectangle_coord[,'X'])
  sf_x_max <- max(rectangle_coord[,'X'])
  x_npixels_init <- floor(abs((sf_x_min - img_x_offset)/img_x_scale))
  x_npixels_last <- ceiling(
    round(
      x = abs((sf_x_max - img_x_offset)/img_x_scale),
      digits = 6)
  )
  x_init_crop_img <- img_x_offset + x_npixels_init*img_x_scale
  x_last_crop_img <- img_x_offset + x_npixels_last*img_x_scale

  # Y offset fixed
  sf_y_min <- min(rectangle_coord[,'Y'])
  sf_y_max <- max(rectangle_coord[,'Y'])
  y_npixels_init <- floor(x = abs((sf_y_max - img_y_offset)/img_y_scale))
  y_npixels_last <- ceiling(
    round(
      x = abs((sf_y_min - img_y_offset)/img_x_scale),
      digits = 6
    )
  )
  y_init_crop_img <- img_y_offset + y_npixels_init*img_y_scale
  y_last_crop_img <- img_y_offset + y_npixels_last*img_y_scale
  c(x_init_crop_img, y_init_crop_img, x_last_crop_img, y_last_crop_img)
}

ee_fix_world_boundary <- function(sfc, crs, epsilon = 0.0001) {
  bbox <- st_bbox(sfc)
  xmin <- bbox['xmin']
  xmax <- bbox['xmax']
  ymin <- bbox['ymin']
  ymax <- bbox['ymax']

  # if (xmax == 180) xmax <- xmax - epsilon
  # if (ymin == -90) ymin <- ymin + epsilon

  new_bbox <- c(xmin,ymin,xmax,ymax)
  class(new_bbox) <- "bbox"
  st_set_crs(st_as_sfc(new_bbox), crs)
}

#' Fix region in ee_as_thumbnail and ee_as_stars
#' @importFrom sf st_intersection st_length st_as_text st_set_crs st_as_sfc
#' @noRd
ee_fix_world_region <- function(sf_image, sf_region, quiet) {
  ee_crs <- st_crs(sf_image)
  fix_region <- suppressMessages(
    st_intersection(sf_image,sf_region)
  )
  st_is_identical <- identical(
    st_length(sf_region),
    st_length(fix_region)
  )
  sf_fix_region <- ee_fix_world_boundary(sfc = fix_region,
                                         crs = ee_crs)
  if (isFALSE(st_is_identical) & !quiet) {
    message('NOTE: region argument does not overlap completely the image, changing ',
            'region \nFrom : ', st_as_text(sf_region),
            '\nTo   : ', st_as_text(sf_fix_region))
  }
  list(geometry = sf_fix_region, equal = st_is_identical)
}

