#' Convert between Python and R objects
#' @param x A python object
#' @family ee_utils functions
#' @return An R object
#' @export
ee_utils_py_to_r <- function(x) {
  p_r <- suppressWarnings(try(py_to_r(x), silent = TRUE))
  if (class(p_r) %in% 'try-error') {
    return(x)
  } else {
    return(p_r)
  }
}


#' Create a zip file from an sf object
#'
#' @param x sf object
#' @param filename data source name
#' @param SHP_EXTENSIONS file extension of the files to save
#' into the zip file. By default: "dbf", "prj", "shp", "shx".
#' @importFrom utils zip
#'
#' @return Character. The full path of the created zip file.
#' @family ee_utils functions
#'
#' @examples
#' \dontrun{
#' library(rgee)
#' library(sf)
#' ee_Initialize(gcs = TRUE)
#'
#' # Create sf object
#' nc <- st_read(system.file("shape/nc.shp", package="sf"))
#' zipfile <- ee_utils_shp_to_zip(nc)
#' }
#' @export
ee_utils_shp_to_zip <- function(x,
                                filename,
                                SHP_EXTENSIONS = c("dbf", "prj", "shp",
                                                   "shx")) {
  # check packages
  ee_check_packages("ee_utils_shp_to_zip", "sf")

  if (missing(filename)) {
    filename <- sprintf("%s%s",tempfile(),'.shp')
  }
  sf::write_sf(obj = x, dsn = filename)
  shp_basename <- gsub("\\.shp$", "", filename)
  shp_filenames <- sprintf("%s.%s", shp_basename, SHP_EXTENSIONS)
  zipname <- sprintf("%s.zip", shp_basename)
  zip(zipfile = zipname, files = shp_filenames, flags = "-j -q")
  zipname
}


#' Wrap an R function in a Python function with the same signature.
#' @author Yuan Tang and J.J. Allaire
#'
#' @description This function could wrap an R function in a Python
#' function with the same signature. Note that the signature of the
#' R function must not contain esoteric Python-incompatible constructs.
#'
#' @note \code{\link[reticulate]{py_func}} has been renamed to ee_utils_pyfunc
#' just to maintain the rgee functions name's style. All recognition
#' for this function must always be given to \pkg{reticulate}.
#' @return A Python function that calls the R function `f` with the same
#' signature.
#' @param f An R function
#'
#' @family ee_utils functions
#'
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_Initialize()
#'
#' # Earth Engine List
#' ee_SimpleList <- ee$List$sequence(0, 12)
#' ee_NewList <- ee_SimpleList$map(
#'   ee_utils_pyfunc(
#'     function(x) {
#'       ee$Number(x)$add(x)
#'     }
#'   )
#' )
#'
#' ee_NewList$getInfo()
#'
#' # Earth Engine ImageCollection
#' constant1 <- ee$Image(1)
#' constant2 <- ee$Image(2)
#' ee_ic <- ee$ImageCollection(c(constant2, constant1))
#' ee_newic <- ee_ic$map(
#'   ee_utils_pyfunc(
#'     function(x) ee$Image(x)$add(x)
#'   )
#' )
#' ee_newic$mean()$getInfo()$type
#' }
#' @export
ee_utils_pyfunc <- reticulate::py_func


#' Search into the Earth Engine Data Catalog
#'
#' @param ee_search_dataset Character that represents the EE dataset ID.
#' @return No return value, called for displaying the Earth Engine dataset in the browser.
#' @examples
#' \dontrun{
#'  library(rgee)
#'
#'  ee_datasets <- c("WWF/HydroSHEDS/15DIR", "WWF/HydroSHEDS/03DIR")
#'  ee_utils_dataset_display(ee_datasets)
#' }
#' @export
ee_utils_dataset_display <- function(ee_search_dataset) {
  if (is.character(ee_search_dataset)) {
    tag_name <- gsub("\\/", "_", ee_search_dataset)
  } else {
    id_ee_obj <- ee_search_dataset$get("system:id")$getInfo()
    tag_name <- gsub("\\/", "_", id_ee_obj)
  }
  db_catalog <- "https://developers.google.com/earth-engine/datasets/catalog/"
  catalog_uri <- paste0(db_catalog, tag_name) %>%
    na.omit() %>%
    as.character()
  for (uri in catalog_uri) {
    browseURL(uri)
  }
  invisible(TRUE)
}



#' Return metadata of a COG tile server
#'
#' @param resource Character that represents a COG tile server file.
#' @param titiler_server Titiler endpoint. Defaults to "https://api.cogeo.xyz/".
#' @return A metadata list for a COG file.
#' @examples
#' \dontrun{
#'  library(rgee)
#'
#'  resource <- "https://s3-us-west-2.amazonaws.com/planet-disaster-data/hurricane-harvey/SkySat_Freeport_s03_20170831T162740Z3.tif"
#'  ee_utils_cog_metadata(resource)
#' }
#' @export
ee_utils_cog_metadata <- function(resource, titiler_server = "https://api.cogeo.xyz/") {
  response <- httr::GET(
    url = sprintf("%s/cog/metadata", titiler_server),
    config = httr::accept_json(),
    query = c(list("url" = resource), visParams)
  )
  httr::content(response, type="application/json")
}
