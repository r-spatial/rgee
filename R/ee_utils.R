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
#' @param titiler_server TiTiler endpoint. Defaults to "https://api.cogeo.xyz/".
#' @param visParams Visualization parameters see "https://api.cogeo.xyz/docs".
#' @return A metadata list for a COG file.
#' @examples
#' \dontrun{
#'  library(rgee)
#'
#' server <- "https://s3-us-west-2.amazonaws.com/planet-disaster-data/hurricane-harvey/"
#' file <- "SkySat_Freeport_s03_20170831T162740Z3.tif"
#' resource <- paste0(server, file)
#' visParams <- list(nodata = 0, expression = "B3, B2, B1", rescale = "3000, 13500")
#' ee_utils_cog_metadata(resource, visParams)
#' }
#' @export
ee_utils_cog_metadata <- function(resource, visParams, titiler_server = "https://api.cogeo.xyz/") {
  response <- httr::GET(
    url = sprintf("%s/cog/metadata", titiler_server),
    config = httr::accept_json(),
    query = c(list("url" = resource), visParams)
  )
  httr::content(response, type="application/json")
}



#' The value of a future or the values of all elements in a container
#'
#' Gets the value of a future or the values of all elements (including futures)
#' in a container such as a list, an environment, or a list environment.
#' If one or more futures is unresolved, then this function blocks until all
#' queried futures are resolved.
#'
#' @author Henrik Bengtsson <https://github.com/HenrikBengtsson/>
#'
#' @param future, x A Future, an environment, a list, or a list environment.
#'
#' @param stdout If TRUE, standard output captured while resolving futures
#' is relayed, otherwise not.
#'
#' @param signal If TRUE, \link[base]{conditions} captured while resolving
#' futures are relayed, otherwise not.
#'
#' @param \dots All arguments used by the S3 methods.
#'
#' @return
#' `value()` of a Future object returns the value of the future, which can
#' be any type of \R object.
#'
#' `value()` of a list, an environment, or a list environment returns an
#' object with the same number of elements and of the same class.
#' Names and dimension attributes are preserved, if available.
#' All future elements are replaced by their corresponding `value()` values.
#' For all other elements, the existing object is kept as-is.
#'
#' If `signal` is TRUE and one of the futures produces an error, then
#' that error is produced.
#'
#' @export
ee_utils_future_value <- function(future, stdout = TRUE, signal = TRUE, ...) {
  ee_check_packages("ee_utils_future_value", "future")
  if (is.list(future)) {
    # if all the elements in a list are of the class SequentialFuture.
    condition1 <- all(
      sapply(future, function(x) any(class(x) %in% "SequentialFuture"))
    )
    if (condition1) {
      lazy_batch_extract <- future %>%
        future::value(stdout = stdout, signal = signal, ...)
      # Is the list a results of run ee_imagecollection_to_local?
      if(is(future, "ee_imagecollection")) {
        dsn <- lapply(lazy_batch_extract, '[[', 1)
        metadata <- lapply(lazy_batch_extract, function(x) attr(x, "metadata"))
        # If metadata is NULL means that the user run:
        # ee_imagecollection_to_local(..., add_metadata=FALSE)
        if (any(sapply(metadata, is.null))) {
          unlist(dsn)
        } else {
          mapply(
            function(x, y) list(dsn = x, metadata = y),
            dsn, metadata,
            SIMPLIFY=FALSE
          )
        }
      } else {
        lazy_batch_extract
      }
    } else {
      stop("Impossible to use ee_utils_future_value in a list ",
           "with elements of a class different from SequentialFuture.")
    }
  } else {
    future %>% future::value(stdout = stdout, signal = signal, ...)
  }
}

#' Store Service account key (SaK) inside the EE folder
#'
#' Copy SaK in the ~/.config/earthengine/$USER.
#'
#' @param sakfile Character. SaK filename.
#' @param users Character. The user related to the SaK file. A SaK
#' file can be related to multiple users.
#' @param delete Logical. If TRUE, the SaK filename is deleted after copy.
#' @param quiet Logical. Suppress info message
#' @examples
#' \dontrun{
#' library(rgee)
#'
#' ee_Initialize()
#'
#' sakfile <- "/home/rgee_dev/sak_file.json"
#' ee_utils_sak_copy(sakfile = sakfile, users = c("csaybar", "ndef"))
#' }
ee_utils_sak_copy <- function(sakfile, users = NULL, delete = FALSE, quiet = FALSE) {
  if (is.null(users)) {
    ee_users <- tryCatch(
      expr = ee_get_earthengine_path(),
      error = function(e) {
        ee_Initialize()
        ee_get_earthengine_path()
      }
    )
  } else {
    ee_users <- sprintf("%s/%s", dirname(ee_get_earthengine_path()), users)
  }

  for (ee_user in ee_users) {
    # 1. Remove previous Sak
    file.remove(list.files(ee_user, pattern = "\\.json$", full.names = TRUE))

    # 2. Copy new SaKfile
    file.copy(
      from = sakfile,
      to = sprintf("%s/rgee_sak.json", ee_user),
      overwrite = TRUE
    )
  }

  if (delete) {
    file.remove(sakfile)
  }

  if (!quiet & length(ee_users) == 1) {
    message(
      sprintf("SaK copy succesfully. Please run ee_Initialize(user = '%s', gcs = TRUE) again.", basename(ee_users))
    )
  } else {
    message("SaK copy successfully")
  }
  sprintf("%s/rgee_sak.json", ee_users)
}
