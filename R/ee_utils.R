#' Convert between Python and R objects
#' @param x A python object
#' @family ee_utils functions
#' @return An R object
#' @export
ee_utils_py_to_r <- function(x) {
  p_r <- suppressWarnings(try(reticulate::py_to_r(x), silent = TRUE))
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

#' Stores a Service account key (SaK) inside the EE folder
#'
#' Copy SaK in the ~/.config/earthengine/$USER.
#'
#' @param sakfile Character. SaK filename. If missing, the SaK of the first user is used.
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
#' # sakfile <- "/home/rgee_dev/sak_file.json"
#' ## Copy sakfile to the users 'csaybar' and 'ndef'
#' # ee_utils_sak_copy(sakfile = sakfile, users = c("csaybar", "ndef"))
#'
#' # # Copy the sakfile of the user1 to the user2 and user3.
#' # ee_utils_sak_copy(users = c("csaybar", "ndef", "ryali93"))
#' }
#' @export
ee_utils_sak_copy <- function(sakfile, users = NULL, delete = FALSE, quiet = FALSE) {
  # Check packages
  ee_check_packages("ee_utils_sak_copy", "googleCloudStorageR")

  # Check if the user exists
  main_ee_dir <- dirname(ee_get_earthengine_path())

  if(is.null(users)) {
    # 1. Remove previous Sak
    ee_path <- ee_get_earthengine_path()
    file.remove(list.files(ee_path, pattern = "\\.json$", full.names = TRUE))

    # 2. Copy new SaKfile
    file.copy(
      from = sakfile,
      to = sprintf("%s/rgee_sak.json", ee_path),
      overwrite = TRUE
    )

    if (delete) {
      file.remove(sakfile)
    }

    if (!quiet) {
      cat("SaK copy successfully")
    }
  } else {
    condition <- dir.exists(sprintf("%s/%s", main_ee_dir, users))
    if (!all(condition)) {
      stop(sprintf("The user %s does not exist.", crayon::bold(users[!condition])))
    }


    if (missing(sakfile)) {
      user_ref <- sprintf("%s/%s", main_ee_dir, users)[1]
      sakfile <- list.files(user_ref, '\\.json$', recursive = TRUE, full.names = TRUE)
      if (length(sakfile) == 0) {
        stop("The first user does not have a Service Account Key (SaK) assigned.")
      }
      other_users <- sprintf("%s/%s", main_ee_dir, users)[-1]
      users <- basename(other_users)
    }

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

    if (!quiet) {
      cat("SaK copy successfully")
    }
    sprintf("%s/rgee_sak.json", ee_users)
  }
}


#' Validate a Service account key (SaK)
#'
#' Validate a Service account key (SaK). local_to_gcs, raster_as_ee,
#' stars_as_ee, and sf_as_ee(via = "gcs_to_asset", ...) need that the SaK
#' have privileges to write/read objects in a GCS bucket.
#'
#' @param sakfile Character. SaK filename.
#' @param bucket Character. Name of the GCS bucket. If bucket is not set,
#' rgee will tries to create a bucket using \code{googleCloudStorageR::gcs_create_bucket}.
#' @param quiet Logical. Suppress info message
#' @examples
#' \dontrun{
#' library(rgee)
#'
#' ee_Initialize(gcs = TRUE)
#'
#' # Check a specific SaK
#' sakfile <- "/home/rgee_dev/sak_file.json"
#' ee_utils_sak_validate(sakfile, bucket = "rgee_dev")
#'
#' # Check the SaK for the current user
#' ee_utils_sak_validate()
#' }
#' @export
ee_utils_sak_validate <- function(sakfile, bucket, quiet = FALSE) {
    ee_check_packages(
        fn_name = "ee_utils_sak_validation",
        packages = c("googleCloudStorageR", "jsonlite")
    )

    if (missing(sakfile)) {
        sakfile <- list.files(
            path = ee_get_earthengine_path(),
            pattern = "\\.json$",
            recursive = TRUE,
            full.names = TRUE
        )[1]
    }

    # Load the GCS credential
    googleCloudStorageR::gcs_auth(sakfile)

    # Read the file to get the project id
    project_id <- jsonlite::read_json(sakfile)$project_id

    if (!quiet) {
        cat(
            cli::rule(
                left = crayon::bold("SaK validator"),
                right = "The test should take ~1 min. Please wait."
            )
        )
        cat("\n")
    }
    bucket_rname <- bucket

    # TEST 02
    demo_data <- data.frame(a = 1:10, b = 1:10)
    result02 <- tryCatch(
        expr = {
            suppressMessages(
                googleCloudStorageR::gcs_upload(
                    file = demo_data,
                    name = "demo_data.csv",
                    bucket = bucket_rname,
                    predefinedAcl = "bucketLevel"
                )
            )
            TRUE
        }, error = function(e) {
          message(e)
          message("\nAn ERROR was raised when rgee tried to write in your GCS bucket.")
          return(FALSE)
        }
    )

    if (!quiet & result02) {
        cat(
            sprintf(
                "%s : %s \n",
                crayon::bold("Upload GCS objects"),
                crayon::green$bold("OK!")
            )
        )
    }

    # Download data
    result03 <- tryCatch(
      expr = {
        suppressMessages(
          googleCloudStorageR::gcs_get_object(
            object_name = "demo_data.csv",
            bucket = bucket_rname,
            saveToDisk = tempfile(fileext = ".csv"),
            overwrite = TRUE
          )
        )
        TRUE
      }, error = function(e) {
        message(e)
        message("\nAn ERROR was raised when GEE tried to write your GCS bucket.")
        return(FALSE)
      }
    )

    if (!quiet & result03) {
      cat(
        sprintf(
          "%s : %s \n",
          crayon::bold("Download GCS objects"),
          crayon::green$bold("OK!")
        )
      )
    }

    # Check GCS and GEE sync
    result04 <- tryCatch(
      expr = {
        demo_sf <- ee_as_sf(
          x = ee$Geometry$Point(c(0, 0 )),
          via = "gcs", container = bucket_rname,
          quiet = TRUE,
          public = FALSE
        )
        suppressMessages(
          googleCloudStorageR::gcs_delete_object(
            object_name = attr(demo_sf, "metadata")$metadata$gcs_name,
            bucket = bucket_rname
          )
        )
        TRUE
      }, error = function(e) {
        message(e)
        message("\nAn ERROR was raised when rgee tried to sync GEE & GCS.")
        return(FALSE)
      }
    )

    if (!quiet & result04) {
      cat(
        sprintf(
          "%s : %s \n",
          crayon::bold("GEE & GCS sync"),
          crayon::green$bold("OK!")
        )
      )
    }

    suppressMessages(
      googleCloudStorageR::gcs_delete_object("demo_data.csv", bucket_rname)
    )

    invisible(TRUE)
}



#' Obtain parameters from a Python string
#' @noRd
get_signature <- function (sigs) {
  sig_names <- names(sigs)
  signature_strings <- lapply(sig_names, function(k) {
    if (identical(sigs[[k]], quote(expr = )))
      k
    else {
      py_value_str <- ifelse(
        is.character(sigs[[k]]),
        paste0("'", sigs[[k]], "'"),
        as.character(reticulate::r_to_py(eval(sigs[[k]]))))
      paste0(k, "=", py_value_str)
    }
  })
  paste(signature_strings, collapse = ", ")
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
ee_utils_pyfunc <- function (f) {
  tryCatch({
    sigs <- formals(f)
    if (is.null(sigs)) {
      func_signature <- func_pass_args <- ""
    }
    else {
      func_signature <- get_signature(sigs)
      func_pass_args <- get_signature(lapply(sigs, function(sig) quote(expr = )))
    }
    decostringfunc <- paste0(
      "\ndef wrap_fn(__deco__):\n  def __magick__(%s):\n",
      "    return __deco__(%s)\n  return __magick__\n"
    )
    wrap_fn_util <- reticulate::py_run_string(
      code = sprintf(decostringfunc, func_signature, func_pass_args)
    )
    wrap_fn_util$wrap_fn(f)
  }, error = function(e) {
    stop(paste0("The R function's signature must not contains esoteric ",
                "Python-incompatible constructs. Detailed traceback: \n",
                e$message))
  })
}
