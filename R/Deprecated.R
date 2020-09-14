#' Passing an Earth Engine Image to Local using getinfo
#' @noRd
#'
ee_image_local_getInfo <- function(image, region, dsn, scale, maxPixels,
                                   container, band_names, quiet) {
  message_deprecated <- c(
    "Downloading images via \"getInfo\" will not be available for rgee version 1.0.8 >." ,
    " Use ee_as_raster(..., via = \"drive\"), or ee_as_raster(..., via = \"gcs\") instead."
  )
  .Deprecated("ee_image_local_drive",msg = message_deprecated)
  # If region is NULL get from images
  if (is.null(region)) {
    if (!quiet) {
      message("region is not specified taking the image's region...")
    }
    region <- image$geometry()
  }

  # If region is NULL get from images
  if (is.null(scale)) {
    scale <- tryCatch(
      expr = image %>%
        ee$Image$projection() %>%
        ee$Projection$nominalScale() %>%
        ee$Number$getInfo(),
      error = function(e) {
        message(paste0(e$message, " Trying only taking the first band ...."))
        image %>%
          ee$Image$select(0) %>%
          ee$Image$projection() %>%
          ee$Projection$nominalScale() %>%
          ee$Number$getInfo()
      })
    message(sprintf("scale argument was set at %s meters.", scale))
  }

  # Getting image ID if it is exist
  image_id <- tryCatch(
    expr = jsonlite::parse_json(image$id()$serialize())$
      scope[[1]][[2]][["arguments"]][["id"]],
    error = function(e) "noid_image"
  )
  if (is.null(image_id)) {
    image_id <- "noid_image"
  }

  # PROJ information about the image
  init_proj <- image$projection()$getInfo()
  init_proj_wkt <- image$projection()$wkt()$getInfo()
  init_proj_nominal <- image$projection()$nominalScale()$getInfo()

  # PROJ information about the region
  sf_region <- ee_as_sf(region) %>%
    sf::st_bbox() %>%
    sf::st_as_sfc()

  # region crs and image crs are equal?, otherwise force it.
  if (sf::st_crs(sf_region)$Name != sf::st_crs(init_proj_wkt)$Name) {
    if(!quiet) {
      message("region and image do not have the same CRS. ",
              "Transforming coordinates of region.... ")
    }
    sf_region <- sf::st_transform(sf_region, init_proj_wkt)
  }

  # Image GEOTRANSFORM
  xScale <- init_proj$transform[[1]]
  yScale <- -abs(init_proj$transform[[5]]) #always negative
  xshearing <- init_proj$transform[[2]]
  yshearing <- init_proj$transform[[4]]

  # getInfo does not support xShearing and yShearing different to zero
  if ((xshearing | yshearing) != 0) {
    stop(
      " 'getInfo' does not support xShearing and yShearing different",
      " to zero. Use 'drive' or 'gcs' instead."
    )
  }

  # Changing scale
  new_xScale <- xScale * scale/init_proj_nominal
  new_yScale <- yScale * scale/init_proj_nominal
  init_proj$transform[[1]] <- new_xScale
  init_proj$transform[[5]] <- new_yScale

  # Image info
  new_image <- image$reproject(
    crs = init_proj_wkt,
    crsTransform = init_proj$transform
  )

  # Estimating the number of pixels (approximately)
  # It is necessary just a single batch? (512x512)
  total_pixel <- ee_approx_number_pixels(sf_region, init_proj)
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

  # Warning message if your image is large than 512 * 512 * 3 pixels
  ntile <- 512
  maxPixels_getInfo <- 1024*1024
  nbatch <- ceiling(sqrt(total_pixel / (ntile * ntile)))
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
  sf_region_fixed <- sf_region_gridded %>%
    sf_as_ee(proj = init_proj_wkt,
             quiet = TRUE) %>%
    ee$FeatureCollection()

  # region parameters display
  ee_geometry_message(region = region, sf_region = sf_region, quiet = quiet)

  # ee$FeatureCollection to ee$List
  region_features <- sf_region_fixed$toList(
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
    ee_image_array <- new_image$sampleRectangle(
      region = feature,
      defaultValue = -9999
    )
    # band_names <- c(band_names,"x", "y")
    ee_image_array_local <- ee_image_array$getInfo()
    ee_image_array_local_data <- ee_image_array_local[["properties"]][band_names]
    nrow_array <- length(ee_image_array_local_data[[1]])
    ncol_array <- length(ee_image_array_local_data[[1]][[1]])

    # Passing from an array to a stars object
    # Create array from a list
    image_array <- array(
      data = unlist(ee_image_array_local_data),
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
      img_transform = init_proj$transform,
      sf_region =  sf_region_batch
    )

    xTranslation_fixed <- init_offset[1]
    yTranslation_fixed <- init_offset[4]
    attr_dim$x$offset <- xTranslation_fixed
    attr_dim$y$offset <- yTranslation_fixed
    attr_dim$x$delta <- new_xScale
    attr_dim$y$delta <- new_yScale
    attr(image_stars, "dimensions") <- attr_dim
    sf::st_crs(image_stars) <- init_proj_wkt
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

    # The stars object have bands dimensions?
    if (!is.null(stars::st_get_dimension_values(mosaic,"bands"))) {
      stars::st_set_dimensions(mosaic, 3, values = band_names)
    }

    # Save results in dsn
    stars::write_stars(mosaic, dsn)
  }
}
