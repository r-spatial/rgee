#' Class EERasterBrick
#'
#' A subclass from RasterBrick that adds the metadata slot.
#'
#' @slot metadata A list with metadata related to the download process.
#'
#' @exportClass EERasterBrick
setClass(
  "EERasterBrick",
  contains="RasterBrick",
  slots=c(metadata="list")
)
