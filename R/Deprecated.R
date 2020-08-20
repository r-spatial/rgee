#' Create an Image manifest upload
#'
#' Create a manifest to upload stars, stars-proxy, RasterLayer, RasterBrick,
#' or RasterStack object to Earth Engine assets folder. The "manifest" is simply a
#' JSON file which describe all the upload parameters. See
#' \url{https://developers.google.com/earth-engine/image_manifest} to get more
#' details.
#'
#' @param img stars* or Raster* object.
#' @param gs_uri Character. GCS full path of the image to upload to Earth Engine assets
#' e.g. gs://rgee_dev/l8.tif
#' @param assetId Character. How to call the file once uploaded
#' to the Earth Engine Asset. e.g. users/datacolecfbf/l8.
#' @param properties List. Set of parameters to be set up as properties
#' of the EE object.
#' @param start_time Character. Sets the start time property (system:time_start)
#' It could be a number (timestamp) or a date.
#' @param end_time Character. Sets the end time property (system:time_end)
#' It could be a number (timestamp) or a date.
#' @param pyramiding_policy Character. The pyramid reduction policy to use.
#' @param returnList Logical. If TRUE will return the "manifest" as a list otherwise
#' will return a JSON file.
#' @param quiet Logical. Suppress info message.
#'
#' @return If \code{returnList} is TRUE a list otherwise a JSON file.
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_Initialize()
#' tif <- system.file("tif/L7_ETMs.tif", package = "stars")
#'
#' # Return a JSON file
#' ee_utils_create_manifest_image(
#'   img = tif,
#'   gs_uri = "gs://rgee_dev/l8.tif",
#'   assetId = "users/datacolecfbf/l8"
#' )
#'
#' # Return a list
#' ee_utils_create_manifest_image(
#'   img = tif,
#'   gs_uri = "gs://rgee_dev/l8.tif",
#'   assetId = "users/datacolecfbf/l8",
#'   returnList = TRUE
#' )
#' }
# ee_utils_create_manifest_image_old <- function(img,
#                                            gs_uri,
#                                            assetId,
#                                            properties = NULL,
#                                            start_time = "1970-01-01",
#                                            end_time = "1970-01-01",
#                                            pyramiding_policy = 'MEAN',
#                                            returnList = FALSE,
#                                            quiet = FALSE) {
#
#   if (inherits(img, "character")) {
#     img <- stars::read_stars(img, proxy = TRUE)
#   }
#
#   if (inherits(img, c("RasterBrick", "RasterStack", "RasterLayer"))) {
#     if (!quiet) {
#       message(
#         "Passing img from Raster* to stars-proxy, if this step take a long time\n",
#         "please use read_stars(..., proxy = TRUE) instead of raster(...)."
#       )
#       img <- stars::st_as_stars(img)
#     }
#   }
#
#   # Creating affine_transform params
#   affine_transform <- attr(img, "dimensions")
#   shear <- img %>%
#     attr("dimensions") %>%
#     attr("raster")
#   nbands <- (affine_transform$band$to - affine_transform$band$from) + 1L
#   if (length(nbands) == 0) nbands <- 1
#   band_names <- affine_transform$band$values
#   if (is.null(band_names)) band_names <- sprintf("b%s", 1:nbands)
#   name <- sprintf("projects/earthengine-legacy/assets/%s", assetId)
#
#   if (is.na(sf::st_crs(img)$wkt)) {
#     stop("x does not have a CRS defined first")
#   }
#
#   # Creating tileset
#   tilesets <- list(
#     crs = sf::st_crs(img)$wkt,
#     sources = list(
#       list(
#         uris = gs_uri,
#         affine_transform = list(
#           scale_x = affine_transform$x$delta,
#           shear_x = shear$affine[1],
#           translate_x = affine_transform$x$offset,
#           shear_y = shear$affine[2],
#           scale_y = affine_transform$y$delta,
#           translate_y = affine_transform$y$offset
#         )
#       )
#     )
#   )
#
#   # from R date to JS timestamp: time_start + time_end
#   time_start <- rdate_to_eedate(start_time, timestamp = TRUE)
#   time_end <- rdate_to_eedate(end_time, timestamp = TRUE)
#
#   # Adding bands
#   bands <- list()
#   for (index in seq_len(length(band_names))) {
#     bands[[index]] <- list(
#       id = band_names[index],
#       tileset_band_index = as.integer((index - 1))
#     )
#   }
#
#   # Putting all together
#   manifest <- list(
#     name = name,
#     tilesets = list(tilesets),
#     bands = bands,
#     pyramiding_policy = pyramiding_policy,
#     properties = properties,
#     start_time = list(seconds = time_start / 1000),
#     end_time = list(seconds = time_end / 1000)
#   )
#   if (is.null(properties)) manifest[["properties"]] <- NULL
#   if (returnList) {
#     manifest
#   } else {
#     ee_utils_create_json(json_path)
#   }
# }
