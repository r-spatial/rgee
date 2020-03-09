#' Create a stars object based on an EE thumbnail image
#'
#' Download EE thumbnail images and read them as stars objects.
#' This function is a wrapper around \code{ee$Image()$getThumbURL()}.
#'
#' @param x EE Image object
#' @param region EE Geometry Rectangle (ee$Geometry$Rectangle)
#' @param dimensions A number or pair of numbers in format XY.
#' @param vizparams A list that contains the visualization parameters.
#' @param crs The EE Image projection e.g. 'EPSG:3857'. WGS84 by default
#' ('EPSG:4326').
#' @param quiet logical; suppress info messages.
#' @details
#'
#' The argument dimensions will define the stars object parameters
#' "from" & "to". It must be a single numeric value or a two-element vector.
#' If not defined, 256 is taken by default as the dimension of x
#' (from 1 to 256), and y scales down proportionally. Huge images
#' might cause plodding connections. See
#' \href{https://developers.google.com/earth-engine/client_server}{Client vs
#' Server} for more details.
#'
#' The vizparams set up the number of bands up. In `ee_as_thumbnail` just is
#' possible export only one (G) or three-band (RGB) images. Additional
#' parameters can be passed on to control color, intensity, the maximum and
#' minimum values, etc. The below table provides all the parameters that
#' admit `ee_as_thumbnail`.
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
#' Use \code{ee$Image()$geometry()$projection()$crs()$getInfo()} for
#' getting the CRS of an Earth Engine Image.
#' @return
#' An \link[sf]{sf} object
#'
#' @importFrom stars st_set_dimensions st_as_stars write_stars
#' @importFrom sf st_crs<-
#' @importFrom reticulate py_to_r
#' @importFrom jpeg readJPEG
#' @importFrom utils download.file zip str
#' @importFrom png readPNG
#' @examples
#' library(rgee)
#' library(stars)
#'
#' ee_reattach() # reattach ee as a reserved word
#' ee_Initialize()
#' nc <- st_read(system.file("shp/arequipa.shp", package = "rgee"))
#' dem_palette <- c(
#'   "#008435", "#1CAC17", "#48D00C", "#B3E34B", "#F4E467",
#'   "#F4C84E", "#D59F3C", "#A36D2D", "#C6A889", "#FFFFFF"
#' )
#' ndvi_palette <- c(
#'   "#FC8D59", "#FC8D59", "#FC9964", "#FED89C", "#FFFFBF",
#'   "#FFFFBF", "#DAEF9F", "#9DD46A", "#91CF60", "#91CF60"
#' )
#' # Example 01  - SRTM AREQUIPA-PERU
#' image <- ee$Image("CGIAR/SRTM90_V4")
#' region <- nc$geometry[[1]] %>%
#'   st_bbox() %>%
#'   st_as_sfc() %>%
#'   st_set_crs(4326) %>%
#'   sf_as_ee()
#'
#' arequipa_dem <- ee_as_thumbnail(x = image, region = region,
#'                                 vizparams = list(min = 0, max = 5000))
#' arequipa_dem <- arequipa_dem * 5000
#'
#' plot(x = arequipa_dem[nc], col = dem_palette, breaks = "equal",
#'      reset = FALSE, main = "SRTM - Arequipa")
#' suppressWarnings(plot(x = nc, col = NA, border = "black", add = TRUE,
#'                       lwd = 1.5))
#' @export
ee_as_thumbnail <- function(x, region, dimensions, vizparams = NULL, crs = 4326,
                            quiet = FALSE) {
  if (!any(class(x) %in%  "ee.image.Image")) {
    stop("x argument is not an ee$Image")
  }
  if (missing(region)) {
    region <- x$geometry()
  }

  # region testing
  sf_region <- ee_as_sf(region)$geometry
  is_geodesic <- region$geodesic()$getInfo()
  sf_image <- ee_as_sf(x$geometry())$geometry

  ## it is geodesic?
  if (is_geodesic && !quiet) {
    message('Are you sure you want to consider line ',
            'segments as spherical geodesics distances?.',
            ' Fix it as follow: sf_as_ee(geom, geodesic = FALSE)')
  }
  ## region and x have the same crs?
  if (!identical(st_crs(sf_image), st_crs(sf_region))) {
   stop('The parameters region and x need to have the same crs\n',
        'EPSG region: ', st_crs(sf_region)$epsg,
        '\nEPSG x: ', st_crs(sf_image)$epsg)
  }
  ## region is a ee$Geometry$Rectangle?
  if (any(class(region) %in% "ee.geometry.Geometry")) {
    npoints <- nrow(st_coordinates(sf_region))
    if (npoints != 5) {
      warning('region needs to be a ee$Geometry$Rectangle. ',
              'Getting bounds ...\n')
      region <- region$bounds()
      sf_region <- ee_as_sf(region)$geometry
    }
  } else  {
    stop('region needs to be a ee$Geometry$Rectangle.')
  }

  ## region is a world scene?
  if (any(st_bbox(sf_region) %in% c(180,-90))) {
    sf_region <- ee_fix_world_region(sf_image, sf_region)$geometry
  }

  if (missing(dimensions)) {
    dimensions <- 256L
    if (!quiet) {
      message("dimensions param is missing. Taken by default 256 pixels in X")
    }
  }
  if (max(dimensions) > 2048) {
    if (!quiet) {
      message("For large image is preferible use rgee::ee_download_*(...)",
              "or rgee::ee_as_stars(...)")
    }
  }

  # Getting image ID if it is exist
  image_id <- tryCatch(
    expr = parse_json(x$id()$serialize())$
      scope[[1]][[2]][["arguments"]][["id"]],
    error = function(e) "thumbnail"
  )

  # Getting image parameters
  init_offset <- ee_fix_offset(x, sf_region)
  ee_crs <- st_crs(sf_region)$epsg
  region_fixed <- sf_as_ee(x = sf_region,
                           check_ring_dir = TRUE,
                           evenOdd = TRUE,
                           proj = ee_crs,
                           geodesic = is_geodesic)

  # Preparing parameters
  new_params <- list(
    crs = paste0('EPSG:', ee_crs),
    dimensions = as.integer(dimensions),
    region = ee$Geometry(region_fixed$geometry())
  )

  viz_params_total <- c(new_params, vizparams)

  # Creating thumbnail as either jpg or png
  if (!quiet) {
    cat(
      "Getting the thumbnail image from Earth Engine ...",
      "please wait\n"
    )
  }

  thumbnail_url <- x$getThumbURL(viz_params_total)
  z <- tempfile()
  download.file(thumbnail_url, z, mode = "wb", quiet = TRUE)

  # Handling problems with respect to the format
  # of the getTHumbURL (sometimes jpeg other png)
  error_message_I <- paste0(
    "Error arise after trying to download ",
    "the getThumbURL (it needs to be ",
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
    band_name <- paste0(c("R", "G", "B"),"_",image_id)
    stars_png <- mapply(read_png_as_stars,
      bands,
      band_name,
      SIMPLIFY = FALSE,
      MoreArgs = list(mtx = raw_image)
    )
    add <- function(x) Reduce(c, x)
    stars_png %>%
      add() %>%
      merge() %>%
      st_set_dimensions(names = c("x", "y", "bands")) ->
      stars_png

    attr_dim <- attr(stars_png, "dimensions")
    attr_dim$x$offset <- min(long)
    attr_dim$y$offset <- max(lat)
    attr_dim$x$delta <- (max(long) - min(long)) / attr_dim$x$to
    attr_dim$y$delta <- (min(lat) - max(lat)) / attr_dim$y$to

    attr(stars_png, "dimensions") <- attr_dim
    st_crs(stars_png) <- ee_x_epsg
    return(stars_png)
  } else if (bands == 1) {
    # Image band name
    band_name <- x$bandNames()$getInfo()
    # Create a stars object for single band image
    stars_png <- mapply(read_png_as_stars,
      bands,
      band_name,
      SIMPLIFY = FALSE,
      MoreArgs = list(mtx = raw_image)
    )[[1]]
    stars_png <- st_set_dimensions(.x = stars_png,
                                   names = c("x", "y","bands"))
    attr_dim <- attr(stars_png, "dimensions")
    attr_dim$x$offset <- init_offset[1]
    attr_dim$y$offset <- init_offset[2]
    attr_dim$x$delta <- (init_offset[3] - init_offset[1]) / attr_dim$x$to
    attr_dim$y$delta <- (init_offset[4] - init_offset[2]) / attr_dim$y$to
    st_crs(stars_png) <- ee_crs
    attr(stars_png, "dimensions") <- attr_dim
    st_set_dimensions(stars_png, 3, values = band_name)
  } else {
    stop('Number of bands not supported')
  }
}

# From R array to stars
read_png_as_stars <- function(x, band_name, mtx) {
  rotate_x <- t(mtx[, , x])
  dim_x <- dim(rotate_x)
  array_x <- array(NA, dim = c(dim_x[1], dim_x[2], 1))
  array_x[,,1] <- rotate_x
  stars_object <- st_as_stars(array_x)
  names(stars_object) <- band_name
  stars_object
}

