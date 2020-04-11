#' Create a stars object based on an EE thumbnail image
#'
#' Wrapper function to create a stars object with projection from a
#' \href{developers.google.com/earth-engine/image_visualization#thumbnail-images
#' }{EE thumbnail image}.
#'
#' @param x EE Image object to be converted into a stars object.
#' @param region EE Geometry Rectangle (\code{ee$Geometry$Rectangle}) specifying
#' the region to export.The CRS needs to be the same as the \code{x} argument,
#' otherwise, it will be forced.
#' @param dimensions Numeric vector of length 2. Thumbnail dimensions in pixel
#' units. If a single integer is provided, it defines the size of the
#' image's larger aspect dimension and scales the smaller dimension
#' proportionally. Defaults to 512 pixels for the larger image aspect dimension.
#' @param vizparams A list that contains the visualization parameters.
#' See details.
#' @param raster Logical. Should the thumbnail image be saved as a
#' raster object?
#' @param quiet logical; suppress info messages.
#' @details
#'
#' \code{vizparams} set up the details of the thumbnail image. With
#' `ee_as_thumbnail` only is possible export one-band (G) or three-band
#' (RGB) images. Several parameters can be passed on to control color,
#' intensity, the maximum and minimum values, etc. The table below provides
#' all the parameters that admit `ee_as_thumbnail`.
#'
#' \tabular{lll}{
#' \strong{Parameter} \tab \strong{Description} \tab \strong{Type}\cr
#' \strong{bands}     \tab Comma-delimited list of
#' three band names to be mapped to RGB \tab  list \cr
#' \strong{min}       \tab  Value(s) to map to 0 \tab
#' number or list of three numbers, one for each band \cr
#' \strong{max}       \tab  Value(s) to map to 1 \tab
#' number or list of three numbers, one for each band \cr
#' \strong{gain}      \tab  Value(s) by which to multiply each pixel value \tab
#' number or list of three numbers, one for each band \cr
#' \strong{bias}      \tab  Value(s) to add to each Digital Number (DN)
#' value \tab number or list of three numbers, one for each band \cr
#' \strong{gamma}     \tab  Gamma correction factor(s) \tab
#' number or list of three numbers, one for each band \cr
#' \strong{palette}  \tab  List of CSS-style color strings
#' (single-band images only) \tab  comma-separated list of hex strings \cr
#' \strong{opacity}   \tab  The opacity of the layer
#' (0.0 is fully transparent and 1.0 is fully opaque) \tab
#' number \cr
#' }
#'
#' @return
#' An stars object
#'
#' @importFrom stars st_set_dimensions st_as_stars write_stars
#' @importFrom methods as
#' @importFrom sf st_crs<-
#' @importFrom reticulate py_to_r
#' @importFrom jpeg readJPEG
#' @importFrom dplyr slice
#' @importFrom utils download.file zip str
#' @importFrom png readPNG
#' @examples
#' \dontrun{
#' library(raster)
#' library(stars)
#' library(rgee)
#' ee_reattach() # reattach ee as a reserved word
#' ee_Initialize()
#' nc <- st_read(system.file("shp/arequipa.shp", package = "rgee"))
#' dem_palette <- c(
#'   "#008435", "#1CAC17", "#48D00C", "#B3E34B", "#F4E467",
#'   "#F4C84E", "#D59F3C", "#A36D2D", "#C6A889", "#FFFFFF"
#' )
#'
#' ## DEM data -SRTM v4.0
#' image <- ee$Image("CGIAR/SRTM90_V4")
#' world_region <- ee$Geometry$Rectangle(
#'   coords = c(-180,-60,180,60),
#'   proj = "EPSG:4326",
#'   geodesic = FALSE
#' )
#'
#' ## world - elevation
#' world_dem <- ee_as_thumbnail(
#'   x = image,
#'   region = world_region,
#'   dimensions = 1024,
#'   vizparams = list(min = 0, max = 5000)
#' )
#'
#' world_dem[world_dem <= 0] <- NA
#' world_dem <- world_dem * 5000
#' plot(
#'   x = world_dem, col = dem_palette, breaks = "equal",
#'   reset = FALSE, main = "SRTM - World"
#' )
#'
#' ## Arequipa-Peru
#' arequipa_region <- nc %>%
#'   st_bbox() %>%
#'   st_as_sfc() %>%
#'   st_set_crs(4326) %>%
#'   sf_as_ee() %>%
#'   ee$FeatureCollection$geometry()
#'
#' arequipa_dem <- ee_as_thumbnail(
#'   x = image,
#'   region = arequipa_region,
#'   dimensions = 512,
#'   vizparams = list(min = 0, max = 5000)
#' )
#'
#' arequipa_dem <- arequipa_dem * 5000
#' plot(
#'   x = arequipa_dem[nc], col = dem_palette, breaks = "equal",
#'   reset = FALSE, main = "SRTM - Arequipa"
#' )
#'
#' suppressWarnings(plot(
#'   x = nc, col = NA, border = "black", add = TRUE,
#'   lwd = 1.5
#' ))
#' dev.off()
#'
#' ## LANDSAT 8
#' img <- ee$Image("LANDSAT/LC08/C01/T1_SR/LC08_038029_20180810")$
#'   select(c("B4", "B3", "B2"))
#' Map$centerObject(img)
#' Map$addLayer(img, list(min = 0, max = 5000, gamma = 1.5))
#'
#' ## Teton Wilderness
#' l8_img <- ee_as_thumbnail(
#'   x = img,
#'   region = img$geometry()$bounds(),
#'   dimensions = 1024,
#'   vizparams = list(min = 0, max = 5000, gamma = 1.5),
#'   raster = TRUE
#' )
#' plotRGB(l8_img, stretch = "lin")
#' }
#' @export
ee_as_thumbnail <- function(x, region, dimensions, vizparams = NULL,
                            raster = FALSE, quiet = FALSE) {

  if (!any(class(x) %in% "ee.image.Image")) {
    stop("x argument is not an ee$image$Image")
  }

  if (!any(class(region) %in% "ee.geometry.Geometry")) {
    stop("region argument is not an ee$geometry$Geometry")
  }

  sf_region <- ee_table_as_sf(x = region)$geometry
  ## region is a ee$Geometry$Rectangle?
  if (any(class(region) %in% "ee.geometry.Geometry")) {
    npoints <- nrow(st_coordinates(sf_region))
    if (npoints != 5) {
      stop(
        stop("region needs to be a ee$Geometry$Rectangle.")
      )
    }
  }

  if (missing(dimensions)) {
    dimensions <- 512L
    if (!quiet) {
      message("dimensions param is missing. Assuming 512",
              " for the larger image aspect dimension.")
    }
  }

  if (max(dimensions) > 4096) {
    if (!quiet) {
      message(
        "For large image is preferible use rgee::ee_download_*(...)",
        "or rgee::ee_as_stars(...)"
      )
    }
  }

  # Getting image ID if it is exist
  image_id <- tryCatch(
    expr = parse_json(x$id()$serialize())$
      scope[[1]][[2]][["arguments"]][["id"]],
    error = function(e) "thumbnail"
  )


  ## is geodesic?
  is_geodesic <- region$geodesic()$getInfo()

  ## is_evenodd?
  query_params <- unlist(parse_json(region$serialize())$scope)
  is_evenodd <- as.logical(
    query_params[grepl("evenOdd", names(query_params))]
  )
  if (length(is_evenodd) == 0 | is.null(is_evenodd)) {
    is_evenodd <- TRUE
  }

  # bbox and CRS
  init_offset <- st_bbox(sf_region)
  ee_crs <- st_crs(sf_region)$epsg

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

  # Preparing parameters
  vizparams$dimensions <- dimensions
  vizparams$region <- region
  if (is.null(vizparams$format)) {
    vizparams$format <- "png"
  }

  # Creating thumbnail as either jpg or png
  if (!quiet) {
    cat(
      "Getting the thumbnail image ... please wait\n"
    )
  }
  thumbnail_url <- x$getThumbURL(vizparams)
  z <- tempfile()
  download.file(thumbnail_url, z, mode = "wb", quiet = TRUE)

  # Handling problems with respect to the format
  # of the getTHumbURL (sometimes jpeg other png)
  error_message_I <- paste0(
    "Error arise after trying to download ",
    "via getThumbURL (it needs to be ",
    "either jpeg or png)"
  )
  raw_image <- tryCatch(
    tryCatch(readPNG(z),
             error = function(e) {
               pre_raw <- readJPEG(z)
               if (length(dim(pre_raw)) == 2) {
                 dim(pre_raw) <- c(dim(pre_raw), 1)
               }
               pre_raw
             }
    ),
    error = function(e) stop(error_message_I)
  )

  # matrix to array
  if (length(dim(raw_image)) == 2) {
    dim(raw_image) <- c(dim(raw_image), 1)
  }

  # It is a RGB or gray image?
  if (dim(raw_image)[3] == 1) {
    # jpeg
    bands <- 1
  } else if (dim(raw_image)[3] == 2) {
    # png
    bands <- 1
  } else if (dim(raw_image)[3] == 3) {
    # png(color) or others
    bands <- 3
  } else if (dim(raw_image)[3] == 4) {
    bands <- 3
  }

  # Create a stars object for RGB images
  if (bands == 3) {
    band_name <- c("R", "G", "B")
    stars_png <- mapply(read_png_as_stars,
                        seq_len(bands),
                        band_name,
                        SIMPLIFY = FALSE,
                        MoreArgs = list(mtx = raw_image)
    )
    add <- function(x) Reduce(c, x)
    stars_png %>%
      add() %>%
      merge() %>%
      slice("X3", 1) %>%
      st_set_dimensions(names = c("x", "y", "band")) -> stars_png


    attr_dim <- attr(stars_png, "dimensions")
    attr_dim$x$offset <- init_offset[1]
    attr_dim$y$offset <- init_offset[2]
    attr_dim$x$delta <- (init_offset[3] - init_offset[1]) / attr_dim$x$to
    attr_dim$y$delta <- (init_offset[4] - init_offset[2]) / attr_dim$y$to

    attr(stars_png, "dimensions") <- attr_dim
    st_crs(stars_png) <- ee_crs
    if (isFALSE(raster)) {
      thumbnail_stars <- st_as_stars(as(stars_png, "Raster"))
      names(thumbnail_stars) <- image_id
      thumbnail_stars <- st_set_dimensions(
        .x = thumbnail_stars,
        which =  3,
        values = band_name
      )
      thumbnail_stars
    } else {
      thumbnail_raster <- as(stars_png, "Raster")
      names(thumbnail_raster) <- band_name
      thumbnail_raster
    }
  } else if (bands == 1) {
    # Image band name
    band_name <- x$bandNames()$getInfo()
    # Create a stars object for single band image
    stars_png <- mapply(read_png_as_stars,
                        bands,
                        image_id,
                        SIMPLIFY = FALSE,
                        MoreArgs = list(mtx = raw_image)
    )[[1]]

    stars_png <- st_set_dimensions(
      .x = stars_png,
      names = c("x", "y", "bands")
    ) %>% st_set_dimensions(which = 3, values = band_name)

    attr_dim <- attr(stars_png, "dimensions")
    attr_dim$x$offset <- init_offset[1]
    attr_dim$y$offset <- init_offset[2]
    attr_dim$x$delta <- (init_offset[3] - init_offset[1]) / attr_dim$x$to
    attr_dim$y$delta <- (init_offset[4] - init_offset[2]) / attr_dim$y$to
    attr(stars_png, "dimensions") <- attr_dim
    st_crs(stars_png) <- ee_crs
    if (isFALSE(raster)) {
      thumbnail_stars <- st_as_stars(as(stars_png, "Raster"))
      names(thumbnail_stars) <- image_id
      thumbnail_stars
    } else {
      thumbnail_raster <- as(stars_png, "Raster")
      names(thumbnail_raster) <- image_id
      thumbnail_raster
    }
  } else {
    stop("Number of bands not supported")
  }
}

# From R array to stars
read_png_as_stars <- function(x, band_name, mtx) {
  rotate_x <- t(mtx[, , x])
  dim_x <- dim(rotate_x)
  array_x <- array(NA, dim = c(dim_x[1], dim_x[2], 1))
  array_x[, , 1] <- rotate_x
  stars_object <- st_as_stars(array_x)
  names(stars_object) <- band_name
  stars_object
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
#' @param quiet logical. Suppress info message
#' @return A list of parameters
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_reattach()
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
  img_proj <- image$projection()$getInfo()
  geotransform <- unlist(img_proj$transform)
  img_totalarea <- ee_table_as_sf(image$geometry())
  bbox <- img_totalarea %>%
    st_transform(as.numeric(gsub("EPSG:", "", img_proj$crs))) %>%
    st_bbox() %>%
    as.numeric()

  x_diff <- bbox[3] - bbox[1]
  y_diff <- bbox[4] - bbox[2]
  x_npixel <- ceiling(abs(x_diff / geotransform[1]))
  y_npixel <- ceiling(abs(y_diff / geotransform[5]))
  total_pixel <- abs(as.numeric(x_npixel * y_npixel))
  if (isFALSE(getsize)) {
    return(invisible(total_pixel))
  }
  if (!quiet) {
    cat('Image Rows       :', x_npixel,'\n')
    cat('Image Cols       :', y_npixel,'\n')
    cat('Number of Pixels :', format(total_pixel,scientific = FALSE),'\n')
  }
  if (isTRUE(getsize)) {
    bandtypes_info <- image$bandTypes()$getInfo()
    img_types <- unlist(bandtypes_info)
    band_types <- img_types[grepl("precision", names(img_types))]
    band_precision <- vapply(band_types, ee_get_typeimage_size, 0)
    number_of_bytes <- total_pixel * band_precision / compression_ratio
    image_size <- sum(number_of_bytes)
    if (!quiet) {
      cat("Image Size       :", ee_humansize(image_size), "\n")
    }
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

ee_get_typeimage_size <- function(type) {
  if (type == "int") {
    32
  } else if (type == "INT") {
    32
  } else if (type == "double") {
    64
  } else if (type == "float") {
    64
  } else if (type == "int8") {
    8
  } else if (type == "int16") {
    16
  } else if (type == "int32") {
    32
  } else {
    32
  }
}
