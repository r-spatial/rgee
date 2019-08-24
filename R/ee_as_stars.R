#' Create a stars object based on a Earth Engine image
#'
#' @param x TODO
#' @param region TODO
#' @param scale TODO
#' @param palette TODO
#' @param min TODO
#' @param max TODO
#' @param crs TODO
#' @importFrom  stars st_set_dimensions st_as_stars write_stars
#' @importFrom  utils download.file zip str
#' @importFrom png readPNG
#' @export
ee_as_stars <- function(x, region, scale, palette = NULL, min = 0, max = 1, crs = 'EPSG:4326') {
  if (class(x)[1] != "ee.image.Image") stop("image is not a ee.image.Image class")

  if (!missing(region)) {
    region <- ee$Geometry$Polygon(region)$getInfo()['coordinates'][[1]][[1]]
  } else {
    region <- x$geometry()$bounds()$getInfo()['coordinates'][[1]][[1]]
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
    stop("the scale must be a single numeric value or a two-element vector which representing the cell size for x and y.")
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

  viz_params <- list(crs = crs,
                     min = min,
                     max = max,
                     palette = palette,
                     dimensions = dimensions,
                     region = new_region)
  thumbnail_url <- x$getThumbURL(viz_params)
  z <- tempfile()
  download.file(thumbnail_url,z,mode="wb",quiet = TRUE)
  raw_image <- readPNG(z)

  bands <- (dim(raw_image)[3]-1):1
  if (length(bands)==3) band_name <- c("R","G","B") else band_name <- "G"

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
}

read_png_as_stars <- function(x,band_name,mtx) {
  rotate_x <- t(mtx[,,x])
  stars_object <- st_as_stars(rotate_x)
  names(stars_object) = band_name
  stars_object
}
