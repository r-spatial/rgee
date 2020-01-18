#' Create a stars object based on an EE thumbnail image
#'
#' Download EE thumbnail images and read them as stars objects.
#' This function is a wrapper around \code{ee$Image()$getThumbURL()}.
#'
#' @param x EE Image object
#' @param region EE Geometry Rectangle (ee$Geometry$Rectangle)
#' @param dimensions A number or pair of numbers in format XY.
#' @param vizparams A list that contains the visualization parameters.
#' @param crs The EE Image projection e.g. 'EPSG:3857'. Defaults to WGS84 ('EPSG:4326').
#' @param quiet logical; suppress info messages.
#' @details
#'
#' The argument dimensions will define the "from" & "to" parameters of the stars object.
#' It must be a single numeric value or a two-element vector. If not defined,
#' 256 is taken by default as the dimension of x (from 1 to 256), and y will
#' be computed by proportional scaling. Huge images might cause plodding connections.
#' See \href{https://developers.google.com/earth-engine/client_server#client-and-server-functions}{Client vs Server}
#' for details.
#'
#' The vizparams set up the number of bands. In `ee_as_thumbnail` just is possible export one (G) or
#' three band (RGB) images, additional  parameters can be passed on to control color, intensity,
#' the max(min) value, etc. See below for a more compressible list of all the parameters that admit
#' `ee_as_thumbnail`.
#'
#' \tabular{lll}{
#' \strong{Parameter}\tab \strong{Description}\tab\strong{Type}\cr
#' \strong{bands}     \tab  Comma-delimited list of three band names to be mapped to RGB                \tab  list                                               \cr
#' \strong{min}       \tab  Value(s) to map to 0                                                        \tab  number or list of three numbers, one for each band \cr
#' \strong{max}       \tab  Value(s) to map to 1                                                      \tab  number or list of three numbers, one for each band \cr
#' \strong{gain}      \tab  Value(s) by which to multiply each pixel value                              \tab  number or list of three numbers, one for each band \cr
#' \strong{bias}      \tab  Value(s) to add to each Digital Number (DN) value                           \tab  number or list of three numbers, one for each band \cr
#' \strong{gamma}     \tab  Gamma correction factor(s)                                                  \tab  number or list of three numbers, one for each band \cr
#' \strong{palette}  \tab  List of CSS-style color strings (single-band images only)                   \tab  comma-separated list of hex strings                \cr
#' \strong{opacity}   \tab  The opacity of the layer (0.0 is fully transparent and 1.0 is fully opaque) \tab  number \cr
#' }
#'
#' Use \code{ee$Image()$geometry()$projection()$crs()$getInfo()} for getting the CRS of an Earth Engine Image.
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
#' ee_reattach() # reattach ee as a reserve word
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
#' arequipa_dem <- ee_as_thumbnail(x = image, region = region, vizparams = list(min = 0, max = 5000))
#' arequipa_dem <- arequipa_dem * 5000
#' plot(arequipa_dem[nc], col = dem_palette, breaks = "equal", reset = FALSE, main = "SRTM - Arequipa")
#' suppressWarnings(plot(nc, col = NA, border = "black", add = TRUE, lwd = 1.5))
#' @export
ee_as_thumbnail <- function(x, region, dimensions, vizparams = NULL, crs = 4326, quiet = FALSE) {
  if (class(x)[1] != "ee.image.Image") stop("x is not a ee.image.Image class")
  ee_crs <- sprintf("EPSG:%s", crs)

  if (missing(region)) {
    stop("It is necessary define a region as ee$Geometry ")
  }

  if (missing(dimensions)) {
    dimensions <- c(256L, 256L)
    if (!quiet) {
      cat(
        " dimensions is missing, 256x256 is taken by default as dimension of x. \n"
      )
    }
  }

  if (max(dimensions) > 5000) {
    if (!quiet) {
      cat(sprintf(
        " For large image (%sx%s) is preferible use rgee::ee_download_*()\n",
        dimensions[1], dimensions[2]
      ))
    }
  }

  # rastergeometry ----------------------------------------------------------
  bound_coord <- region$
    coordinates()$
    getInfo() %>%
    ee_py_to_r() %>%
    "[["(1)

  long <- sapply(bound_coord, function(x) x[1]) %>% as.numeric()
  lat <- sapply(bound_coord, function(x) x[2]) %>% as.numeric()

  world_lat <- c(-90, -90,  90,  90, -90)
  world_long <- c(-180,  180,  180, -180, -180)

  if (!all(long %in% world_long & lat %in% world_lat)) {
    new_params <- list(
      crs = ee_crs,
      dimensions = as.integer(dimensions),
      region = region
    )
  } else {
    new_params <- list(
      crs = ee_crs,
      dimensions = as.integer(dimensions)
    )

  }

  viz_params_total <- c(new_params, vizparams)

  if (!quiet) cat("Getting the thumbnail image from Earth Engine ... please wait\n")
  thumbnail_url <- x$getThumbURL(viz_params_total)
  z <- tempfile()
  download.file(thumbnail_url, z, mode = "wb", quiet = TRUE)

  # -----------------------------------------------
  # Handling problems with respect to the format
  # of the getTHumbURL (sometimes jpeg other png)
  error_message_I <- paste0(
    "Error arise after try to download",
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

  # -----------------------------------------------
  if (bands > 2) {
    if (bands == 3) band_name <- c("R", "G", "B")
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
    plot(stars_png)
    attr_dim <- attr(stars_png, "dimensions")
    attr_dim$x$offset <- min(long)
    attr_dim$y$offset <- max(lat)
    attr_dim$x$delta <- (max(long) - min(long)) / attr_dim$x$to
    attr_dim$y$delta <- (min(lat) - max(lat)) / attr_dim$y$to

    attr(stars_png, "dimensions") <- attr_dim
    st_crs(stars_png) <- crs
    return(stars_png)
  } else {
    band_name <- "G"
    stars_png <- mapply(read_png_as_stars,
      bands,
      band_name,
      SIMPLIFY = FALSE,
      MoreArgs = list(mtx = raw_image)
    )[[1]]
    stars_png %>% st_set_dimensions(names = c("x", "y")) -> stars_png
    attr_dim <- attr(stars_png, "dimensions")
    attr_dim$x$offset <- min(long)
    attr_dim$y$offset <- max(lat)
    attr_dim$x$delta <- (max(long) - min(long)) / dimensions[1]
    attr_dim$y$delta <- (min(lat) - max(lat)) / dimensions[2]
    attr(stars_png, "dimensions") <- attr_dim
    st_crs(stars_png) <- crs
    return(stars_png)
  }
}

# From R array to stars
read_png_as_stars <- function(x, band_name, mtx) {
  rotate_x <- t(mtx[, , x])
  stars_object <- st_as_stars(rotate_x)
  names(stars_object) <- band_name
  stars_object
}


#' From a R spatial object to a valid region for ee_as_thumbnail
#' @noRd
#' @return a list that look like:
#' [[1]]
#' [1] xmin ymin
#' [[2]]
#' [1] xmin  ymax
#' [[3]]
#' [1] xmax  ymax
#' [[4]]
#' [1] xmax ymin
#' [[5]]
#' [1] xmin ymin
#'
create_region <- function(x) {
  if (any(class(x) %in% "sfg")) {
    region <- sf_as_ee(x)$getInfo()[["coordinates"]]
  } else if (any(class(x) %in% "ee.geometry.Geometry")) {
    region <- x$getInfo()[["coordinates"]]
  } else if (any(class(x) %in% "numeric")) {
    xmin <- x[1]
    xmax <- x[3]
    ymin <- x[2]
    ymax <- x[4]
    coord_x <- c(xmin, ymin, xmin, ymax, xmax, ymax, xmax, ymin, xmin, ymin)
    sfg_obj <- st_polygon(list(matrix(coord_x, ncol = 2, byrow = TRUE)))
    region <- sf_as_ee(sfg_obj)$getInfo()["coordinates"]
  }
  invisible(ee_py_to_r(region)[[1]])
}

