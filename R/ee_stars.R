#' Convert an Earth Engine (EE) image in a stars object
#' @param image ee$Image to be converted into a sf object
#' @param region ee$Geometry$Polygon. Region of interest
#' @param quiet logical. Suppress info message
#' @importFrom jsonlite parse_json
#' @examples
#' library(raster)
#' library(stars)
#' library(rgee)
#' # Define an image.
#' img <- ee$Image('LANDSAT/LC08/C01/T1_SR/LC08_038029_20180810')$
#'           select(c('B4', 'B5', 'B6'))
#' # Define an area of interest.
#' geometry <- ee$Geometry$Rectangle(c(-110.8, 44.6, -110.6, 44.7))
#'
#' # Download Image an area of interest.
#' img_stars <- ee_as_star(image = img,region = geometry)
#' image(img_stars, rgb = c(3,2,1))
#' as(img_stars,'Raster') %>% plotRGB(3,2,1)
#' @export
ee_as_star <- function(image, region, quiet=FALSE) {
  if (!quiet) {
    cat('NOTE: The max number of pixels to ',
            'transfer by this method is around ',
            '262144 (512x512) consider to use ',
            'ee_image_to_*() for more large images.')
  }
  ee_image_array <- image$sampleRectangle(region = region)
  band_names <- image$bandNames()$getInfo()
  band_results <- list()
  for (index in seq_along(band_names)) {
    band <- band_names[index]
    band_results[[band]] <- ee_image_array$get(band)$getInfo()
    if (index == 1) {
      nrow_array <- length(band_results[[band]])
      ncol_array <- length(band_results[[band]][[1]])
    }
  }
  image_array <- array(data = unlist(band_results),
                       dim = c(ncol_array, nrow_array, length(band_names)))
  image_id <- tryCatch(
    expr = parse_json(image$id()$serialize())$
      scope[[1]][[2]][["arguments"]][["id"]],
    error = function(e) "ee_as_star"
  )

  # 1. Create a stars object from a array
  image_stars <- image_array %>%
    st_as_stars %>%
    `names<-`(image_id) %>%
    st_set_dimensions(names = c("x", "y","bands"))
  attr_dim <- attr(image_stars, "dimensions")

  # 2. Set Geotransform
  img_coordinates <- region$coordinates()$getInfo()[[1]]
  long <- vapply(img_coordinates, function(x) x[1], FUN.VALUE = 0)
  lat <- vapply(img_coordinates, function(x) x[2], FUN.VALUE = 0)
  dimensions <- dim(image_stars)

  attr_dim$x$offset <- min(long)
  attr_dim$y$offset <- max(lat)
  attr_dim$x$delta <- (max(long) - min(long)) / dimensions[1]
  attr_dim$y$delta <- (min(lat) - max(lat)) / dimensions[2]
  attr(image_stars, "dimensions") <- attr_dim
  image_crs <- gsub('EPSG:','',image$geometry()$projection()$crs()$getInfo())
  st_crs(image_stars) <- as.numeric(image_crs)
  image_stars
}
