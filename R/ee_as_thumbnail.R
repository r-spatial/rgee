#' Create a stars object based on an Earth Engine (EE) thumbnail image
#'
#' Wrapper function to download RGB image results
#'
#' @param x EE Image object
#' @param region Geospatial region of the image (E,S,W,N, EE Geometry,
#' GeoJSON, or sfg). By default, the whole image will be used.
#' @param scale pixel size.
#' @param vizparams A list that contains the visualization parameters. See details.
#' @param crs The target projection e.g. 'EPSG:3857'. Defaults to WGS84 ('EPSG:4326').
#' @param quiet logical; suppress info message.
#' @details
#' The `ee_as_thumbnail` function shares parameters with \href{rgee}{eemap}. This parameters are:
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
#' @importFrom sf st_crs
#' @importFrom reticulate py_to_r
#' @importFrom  utils download.file zip str
#' @importFrom png readPNG
#' @examples
#' \dontrun{
#' library(sf)
#' library(rgee)
#' library(stars)
#' library(cptcity)
#' library(rnaturalearth)
#' ee_Initialize()
#' world_map <- ne_countries(returnclass = "sf")[-7,][['geometry']]  #Remove Antarctica
#'
#' dem_palette <- cpt(pal = "td_DEM_screen",n=10)
#' ndvi_palette <- cpt(pal = "cb_div_RdYlGn_03",n=10)
#'
#'
#' # This nice example has been adapted from https://github.com/kmarkert/cartoee
#' # Example #01  - WORLD DEM
#' image <- ee$Image('CGIAR/SRTM90_V4')
#' region <- c(-180,-90,180,90)
#'
#' world_dem <- ee_as_thumbnail(x = image,region=region,vizparams = list(min=0,max=5000))
#' world_dem <- world_dem*5000
#'
#' plot(world_dem[world_map],col=dem_palette,breaks='equal',reset=FALSE,main='World Elevation')
#' plot(world_map, col = NA, border = 'black', add = TRUE, lwd = 1.5)
#'
#' # Example #02  - WORLD NDVI
#' # function to add NDVI band to imagery
#' calc_ndvi <- function(img) {
#'   ndvi <- img$normalizedDifference(c('Nadir_Reflectance_Band2', 'Nadir_Reflectance_Band1'))
#'   img$addBands(ndvi$rename('ndvi'))
#' }
#'
#' # MODIS Nadir BRDF-Adjusted Reflectance with NDVI band
#' modis = ee$ImageCollection('MODIS/006/MCD43A4')$
#'   filterDate('2010-01-01','2016-01-01')$
#'   map(calc_ndvi)
#' visParams = list(min = -0.5, max = 0.85, bands = 'ndvi')
#' modis_ndvi <- ee_as_thumbnail(x = modis$mean(),vizparams = visParams,region)
#' plot(modis_ndvi[world_map], reset = FALSE, col = ndvi_palette,main = 'World NDVI')
#' plot(world_map, col = NA, border = 'black', add = TRUE, lwd = 1.5)
#' }
#' @export
ee_as_thumbnail <- function(x, region, scale, vizparams = NULL, crs = 4326, quiet=TRUE) {
  if (class(x)[1] != "ee.image.Image") stop("image is not a ee.image.Image class")
  ee_crs <- sprintf("EPSG:%s",crs)
  if (missing(region)) {
    region <- x$geometry()$bounds()$getInfo()['coordinates'][[1]][[1]]
    if (!quiet) warning("Region is missing, all whole image is used: \n",
              "Region estimated: ",paste(region,collapse = " "))
  } else {
    region <- create_region(region)
  }

  mapR_df <- data.frame(do.call(rbind,region))
  min_max_x <- c(min(mapR_df[1]),max(mapR_df[1]))
  min_max_y <- c(min(mapR_df[2]),max(mapR_df[2]))

  if (missing(scale)) {
    scale <- c(512/diff(min_max_x), 512/diff(min_max_y))
    if (!quiet) warning("scale is missing, dimensions of 512x512 taking by default. \n",
                        "scale estimated: ",paste(scale,collapse = " "))
  }

  if (length(scale) == 1L) {
    scale_x = scale
    scale_y = scale
  } else if(length(scale) == 2L) {
    scale_x = scale[1]
    scale_y = scale[2]
  } else {
    stop("The scale must be a single numeric value or a two-element vector",
         ", which represents the cell size for x and y.")
  }

  dim_x <- diff(min_max_x)/scale_x
  dim_y <- diff(min_max_y)/scale_y

  if (dim_y > 5000 | dim_x > 5000 ) {
    if (!quiet) warning(sprintf(" For large image (%sx%s) is preferible use rgee::ee_download_*()",
                                dim_x,dim_y))
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

  new_params <- list(crs = ee_crs,
                     dimensions = dimensions,
                     region = new_region)
  viz_params <- c(new_params, vizparams)

  if (!quiet) cat("Getting the thumbnail image fromm Earth Engine ... please wait")
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
    st_crs(stars_png) <- crs
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
    st_crs(stars_png) <- crs
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
