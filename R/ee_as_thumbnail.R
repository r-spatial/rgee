
#' Create an R spatial gridded object from an EE thumbnail image
#'
#' Wrapper function around \code{ee$Image$getThumbURL} to create a stars or
#' RasterLayer R object from a
#' \href{ https://developers.google.com/earth-engine/guides/image_visualization}{EE thumbnail image}.
#'
#' @param image EE Image object to be converted into a stars object.
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
#' RasterStack object?
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
#' @return An stars or Raster object depending on the \code{raster} argument.
#' @family image download functions
#'
#' @importFrom methods as
#' @importFrom reticulate py_to_r
#' @importFrom utils download.file zip str
#' @examples
#' \dontrun{
#' library(raster)
#' library(stars)
#' library(rgee)
#'
#' ee_Initialize()
#'
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
#'   image = image,
#'   region = world_region,
#'   dimensions = 1024,
#'   vizparams = list(min = 0, max = 5000)
#' )
#'
#' world_dem[world_dem <= 0] <- NA
#' world_dem <- world_dem * 5000
#'
#' plot(
#'   x = world_dem, col = dem_palette, breaks = "equal",
#'   reset = FALSE, main = "SRTM - World"
#' )
#'
#' ## Arequipa-Peru
#' arequipa_region <- nc %>%
#'   st_bbox() %>%
#'   st_as_sfc() %>%
#'   sf_as_ee()
#'
#' arequipa_dem <- ee_as_thumbnail(
#'   image = image,
#'   region = arequipa_region$buffer(1000)$bounds(),
#'   dimensions = 512,
#'   vizparams = list(min = 0, max = 5000)
#' )
#'
#' arequipa_dem <- arequipa_dem * 5000
#' st_crs(arequipa_dem) <- 4326
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
#'   image = img,
#'   region = img$geometry()$bounds(),
#'   dimensions = 1024,
#'   vizparams = list(min = 0, max = 5000, gamma = 1.5),
#'   raster = TRUE
#' )
#' crs(l8_img) <- "+proj=longlat +datum=WGS84 +no_defs"
#' plotRGB(l8_img, stretch = "lin")
#' }
#' @export
ee_as_thumbnail <- function(image, region, dimensions, vizparams = NULL,
                            raster = FALSE, quiet = FALSE) {
  if (!quiet) {
    message_deprecated <- c(
      "NOTE: ee_as_thumbnail can not determine the level of the scale. Is ",
      "possible that the results present geometric offset. ",
      "See https://developers.google.com/earth-engine/guides/scale/ to get ",
      "more details."
    )
    message(message_deprecated)
  }

  # check packages dependencies
  ee_check_packages(
    fn_name = "ee_as_thumbnail",
    packages = c("sf", "stars", "raster", "jsonlite", "png")
  )

  # check viz parameters
  ee_check_vizparam(vizparams)

  # is image an ee.image.Image?
  if (!any(class(image) %in% "ee.image.Image")) {
    stop("image argument is not an ee$image$Image")
  }

  # is region an ee.geometry.Geometry?
  if (!any(class(region) %in% "ee.geometry.Geometry")) {
    stop("region argument is not an ee$geometry$Geometry")
  }

  # From ee$Geometry$Rectangle to sf
  sf_region <- ee_as_sf(x = region)["geometry"]

  ## region is a ee$Geometry$Rectangle?
  if (any(class(region) %in% "ee.geometry.Geometry")) {
    npoints <- nrow(sf::st_coordinates(sf_region))
    if (npoints != 5) {
      stop(
        stop("region needs to be a ee$Geometry$Rectangle.")
      )
    }
  }

  # is dimensions missing?
  if (missing(dimensions)) {
    dimensions <- 512L
    if (!quiet) {
      message("dimensions param is missing. Assuming 512",
              " for the larger image aspect dimension.")
    }
  }

  # it is a large image?
  if (max(dimensions) > 2048) {
    if (!quiet) {
      message(
        "For large image is preferible use rgee::ee_download_*(...)",
        "or rgee::ee_as_*(...)"
      )
    }
  }

  # Getting image ID if it is exist
  image_id <- tryCatch(
    expr = jsonlite::parse_json(image$id()$serialize())$
      scope[[1]][[2]][["arguments"]][["id"]],
    error = function(e) "thumbnail"
  )
  if (is.null(image_id))  image_id <- "thumbnail"

  # Metadata of the Geometry to display
  ## is geodesic?
  is_geodesic <- ee_utils_py_to_r(region$geodesic()$getInfo())
  ## is_evenodd?
  query_params <- unlist(jsonlite::parse_json(region$serialize())$scope)
  is_evenodd <- as.logical(
    query_params[grepl("evenOdd", names(query_params))]
  )
  if (length(is_evenodd) == 0 | is.null(is_evenodd)) {
    is_evenodd <- TRUE
  }

  # bbox and CRS of the geometry
  init_offset <- sf::st_bbox(sf_region)
  ee_crs <- sf::st_crs(sf_region)$epsg

  if (!quiet) {
    ee_geometry_message(region = region,
                        sf_region = sf_region[["geometry"]])
  }

  # Preparing parameters
  vizparams$dimensions <- dimensions
  vizparams$region <- region
  vizparams$format <- "png"
  if (is.null(vizparams$min)) {
    vizparams$min <- 0
  }
  if (is.null(vizparams$max)) {
    vizparams$min <- 1
  }

  # Creating thumbnail in png format
  if (!quiet) {
    cat(
      "Getting the thumbnail image ... please wait\n"
    )
  }
  thumbnail_url <- image$getThumbURL(vizparams)

  # Reading the png image
  z <- tempfile()
  download.file(thumbnail_url, z, mode = "wb", quiet = TRUE)
  raw_image <- png::readPNG(z)

  # matrix to array
  if (length(dim(raw_image)) == 2) {
    dim(raw_image) <- c(dim(raw_image), 1)
  }

  # It is a RGB or gray image?
  if (dim(raw_image)[3] == 1) {
    bands <- 1
  } else if (dim(raw_image)[3] == 2) {
    bands <- 1
  } else if (dim(raw_image)[3] == 3) {
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
      merge()  %>%
      stars::st_set_dimensions(names = c("x", "y", "band")) -> stars_png

    attr_dim <- attr(stars_png, "dimensions")
    attr_dim$x$offset <- init_offset[1]
    attr_dim$y$offset <- init_offset[2]
    attr_dim$x$delta <- (init_offset[3] - init_offset[1]) / attr_dim$x$to
    attr_dim$y$delta <- (init_offset[4] - init_offset[2]) / attr_dim$y$to

    attr(stars_png, "dimensions") <- attr_dim
    sf::st_crs(stars_png) <- ee_crs
    if (isFALSE(raster)) {
      thumbnail_stars <- stars::st_as_stars(as(stars_png, "Raster"))
      names(thumbnail_stars) <- image_id
      thumbnail_stars <- stars::st_set_dimensions(
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
    # Create a stars object for single band image
    stars_png <- mapply(read_png_as_stars,
                        bands,
                        image_id,
                        SIMPLIFY = FALSE,
                        MoreArgs = list(mtx = raw_image)
    )[[1]]
    stars_png <- stars::st_set_dimensions(.x = stars_png, names = c("x", "y"))

    attr_dim <- attr(stars_png, "dimensions")
    attr_dim$x$offset <- init_offset[1]
    attr_dim$y$offset <- init_offset[2]
    attr_dim$x$delta <- (init_offset[3] - init_offset[1]) / attr_dim$x$to
    attr_dim$y$delta <- (init_offset[4] - init_offset[2]) / attr_dim$y$to
    attr(stars_png, "dimensions") <- attr_dim
    sf::st_crs(stars_png) <- ee_crs
    if (isFALSE(raster)) {
      thumbnail_stars <- stars_png %>%
        as("Raster") %>%
        stars::st_as_stars()
      names(thumbnail_stars) <- image_id
      thumbnail_stars
    } else {
      thumbnail_raster <- stars_png %>%
        as("Raster")
      names(thumbnail_raster) <- image_id
      thumbnail_raster
    }
  } else {
    stop("Number of bands not supported")
  }
}

#' From R array to stars
#' @noRd
read_png_as_stars <- function(x, band_name, mtx) {
  rotate <- function(x) t(apply(x, 2, rev))
  rotate_x <- rotate(mtx[, , x])
  dim_x <- dim(rotate_x)
  array_x <- array(NA, dim = c(dim_x[1], dim_x[2], 1))
  array_x[, , 1] <- rotate_x
  stars_object <- stars::st_as_stars(array_x)
  stars_object <- stars_object[, , , 1, drop = TRUE]
  names(stars_object) <- band_name
  stars_object
}


#' Check the visualization parameters
#' @noRd
ee_check_vizparam <- function(x) {
  list_names <- c(
    "bands", "min", "max", "gain", "bias", "gamma", "palette","opacity"
  )
  check_listnames <- names(x) %in% list_names
  if (any(!check_listnames)) {
    stop(
      "The following visualization parameters are not valid: ",
      paste(names(x[!check_listnames]), collapse = ", ")
    )
  }
}
