#' Create a stars object based on a Earth Engine thumbnail
#'
#' @param x Earth Engine Image object
#' @param region Geospatial region of the image (E,S,W,N, earth engine geometry object,
#' GeoJSON, or sfg). By default, the whole image will be used.
#' @param scale spatial pixel size.
#' @param vizparams list; visualization parameters. See Details.
#' @param crs The target projection e.g. 'EPSG:3857'. Defaults to WGS84 ('EPSG:4326').
#'
#' @details
#' The `ee_as_thumbnail` function shares parameters with `ee_map`. The parameters are:
#'
#' \tabular{lll}{
#' \strong{Parameter}\tab \strong{Description}\tab\strong{Type}\cr
#' \strong{bands}     \tab  Comma-delimited list of three band names to be mapped to RGB                \tab  list                                               \cr
#' \strong{min}       \tab  Value(s) to map to 0                                                        \tab  number or list of three numbers, one for each band \cr
#' \strong{max}       \tab  Value(s) to map to 255                                                      \tab  number or list of three numbers, one for each band \cr
#' \strong{gain}      \tab  Value(s) by which to multiply each pixel value                              \tab  number or list of three numbers, one for each band \cr
#' \strong{bias}      \tab  Value(s) to add to each DN                                                  \tab  number or list of three numbers, one for each band \cr
#' \strong{gamma}     \tab  Gamma correction factor(s)                                                  \tab  number or list of three numbers, one for each band \cr
#' \strong{palette}  \tab  List of CSS-style color strings (single-band images only)                   \tab  comma-separated list of hex strings                \cr
#' \strong{opacity}   \tab  The opacity of the layer (0.0 is fully transparent and 1.0 is fully opaque) \tab  number                                             \cr
#' \strong{format}    \tab  Either "jpg" or "png"                                                       \tab  string \cr
#' }
#'
#' @importFrom  stars st_set_dimensions st_as_stars write_stars
#' @importFrom reticulate py_to_r
#' @importFrom  utils download.file zip str
#' @importFrom png readPNG
#' @examples
#' \dontrun{
#' library(rgee)
#' library(sf)
#' library(stars)
#' ee_Initialize()
#'
#' # Fetch a digital elevation model.
#' image <- ee$Image('CGIAR/SRTM90_V4')
#'
#' # params
#' palette <- c('00A600','63C600','E6E600','E9BD3A','ECB176','EFC2B3','F2F2F2')
#' vizparams1 <- list(min=0, max=3000)
#' vizparams2 <- list(min=0, max=3000, palette=palette)
#' outer = list(matrix(c(-84.6, 15.7,-84.6, -55.9,-32.9, -55.9,-32.9, 15.7,-84.6, 15.7),
#'                     ncol=2, byrow=TRUE))
#' region <- st_polygon(outer) %>% sf_as_ee
#' # plot1
#' my_thumbnail <- ee_as_thumbnail(x = image,region = region,scale = 1, vizparams = vizparams1)
#' plot(my_thumbnail)
#'
#' # plot2
#' my_thumbnail <- ee_as_thumbnail(x = image,region = region,scale = 1,vizparams = vizparams2)
#' image(my_thumbnail, rgb = c(3,2,1))
#' }
#' @export
ee_as_thumbnail <- function(x, region, scale, vizparams = NULL, crs = 'EPSG:4326') {
  if (class(x)[1] != "ee.image.Image") stop("image is not a ee.image.Image class")

  if (missing(region)) {
    region <- x$geometry()$bounds()$getInfo()['coordinates'][[1]][[1]]
  } else {
    region <- create_region(region)
  }

  mapR_df <- data.frame(do.call(rbind,region))
  min_max_x <- c(min(mapR_df[1]),max(mapR_df[1]))
  min_max_y <- c(min(mapR_df[2]),max(mapR_df[2]))

  if (length(scale) == 1L) {
    scale_x = scale
    scale_y = scale
  } else if(length(scale) == 2L) {
    scale_x = scale[1]
    scale_y = scale[2]
  } else {
    stop("the scale must be a single numeric value or a two-element ",
         "vector which representing the cell size for x and y.")
  }

  dim_x <- diff(min_max_x)/scale_x
  dim_y <- diff(min_max_y)/scale_y

  if (dim_y > 5000 | dim_x > 5000 ) {
    warning(sprintf(" For large image (%sx%s) is preferible use rgee::ee_download_*()",dim_x,dim_y))
  }

  dimensions <- c(as.integer(ceiling(dim_x)),
                  as.integer(ceiling(dim_y)))

  offset_x <- min(mapR_df[1])
  offset_y <- max(mapR_df[2])

  scale_y <- scale_y*-1
  xmin <- offset_x
  xmax <- offset_x + dimensions[1]*scale_x
  ymin <- offset_y + dimensions[2]*scale_y
  ymax <- offset_y

  new_region <- list(c(xmin,ymin),c(xmin,ymax),c(xmax,ymax),c(xmax,ymin),c(xmin,ymin))

  new_params <- list(crs = crs,
                     dimensions = dimensions,
                     region = new_region)
  viz_params <- c(new_params, vizparams)

  cat("Getting the thumbnail ... please wait")
  print(viz_params)
  thumbnail_url <- x$getThumbURL(viz_params)
  z <- tempfile()
  download.file(thumbnail_url,z,mode="wb",quiet = TRUE)
  raw_image <- readPNG(z)
  bands <- (dim(raw_image)[3]-1):1
  if (length(bands)>2) {
    if (length(bands)==3) band_name <- c("R","G","B")
    stars_png <- mapply(read_png_as_stars,
                        bands,
                        band_name,
                        SIMPLIFY = FALSE,
                        MoreArgs = list(mtx=raw_image))
    add <- function(x) Reduce(c, x)
    stars_png %>%
      add %>%
      merge %>%
      st_set_dimensions(names = c("x","y","bands")) ->
      stars_png

    attr_dim <- attr(stars_png,"dimensions")
    attr_dim$x$offset <- offset_x
    attr_dim$y$offset <- offset_y
    attr_dim$x$delta <- scale_x
    attr_dim$y$delta <- scale_y
    attr(stars_png,"dimensions") <- attr_dim
    return(stars_png)
  } else {
    band_name <- "G"
    stars_png <- mapply(read_png_as_stars,
                        bands,
                        band_name,
                        SIMPLIFY = FALSE,
                        MoreArgs = list(mtx=raw_image))[[1]]
    stars_png %>% st_set_dimensions(names = c("x","y")) -> stars_png

    attr_dim <- attr(stars_png,"dimensions")
    attr_dim$x$offset <- offset_x
    attr_dim$y$offset <- offset_y
    attr_dim$x$delta <- scale_x
    attr_dim$y$delta <- scale_y
    attr(stars_png,"dimensions") <- attr_dim
    return(stars_png)
  }
}

read_png_as_stars <- function(x,band_name,mtx) {
  rotate_x <- t(mtx[,,x])
  stars_object <- st_as_stars(rotate_x)
  names(stars_object) = band_name
  stars_object
}

#' Create a valid region for ee_as_thumbnail
#' @noRd
#' @return a list like:
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
  if (any(class(x) %in%  "sfg")) {
    region <- sf_as_ee(x)$getInfo()['coordinates']
  } else if (any(class(x) %in%  "ee.geometry.Geometry")) {
    region <- x$getInfo()['coordinates']
  } else if (any(class(x) %in%  "geojson")) {
    oauth_func_path <- system.file("python/sf_as_ee.py", package = "rgee")
    sf_as_ee <- ee_source_python(oauth_func_path)
    region <- sf_as_ee$sfg_as_ee_py(x)$getInfo()['coordinates']
  } else if(any(class(x) %in%  "numeric")) {
    xmin <- x[1]
    xmax <- x[3]
    ymin <- x[2]
    ymax <- x[4]
    coord_x <- c(xmin,ymin,xmin,ymax,xmax,ymax,xmax,ymin,xmin,ymin)
    sfg_obj = st_polygon(list(matrix(coord_x,ncol=2, byrow=TRUE)))
    region <- sf_as_ee(sfg_obj)$getInfo()['coordinates']
  }
  invisible(py_to_r(region)[[1]])
}
