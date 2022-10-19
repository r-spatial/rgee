#' Create credentials - Google Drive
#' @noRd
ee_create_credentials_drive <- function(user=NULL, ee_utils, quiet) {
  # check googledrive R package installation
  ee_check_packages("ee_Initialize", "googledrive")

  # setting drive folder
  if (is.null(user)) {
    ee_path <- ee_utils_py_to_r(ee_utils$ee_path())
    ee_path_user <- ee_path
  } else {
    ee_path <- ee_utils_py_to_r(ee_utils$ee_path())
    ee_path_user <- sprintf("%s/%s", ee_path, user)
  }

  # Load GD credentials (googledrive::drive_auth)
  repeat {
    full_credentials <- list.files(path = ee_path_user, full.names = TRUE)
    drive_condition <- grepl(".*_.*@.*", basename(full_credentials))

    # If the googledrive credential does not exist run googledrive::drive_auth
    if (!any(drive_condition)) {
      suppressMessages(
        googledrive::drive_auth(
          email = NULL,
          cache = ee_path_user
        )
      )
    } else {
      drive_credentials <- full_credentials[drive_condition]
      email <- sub("^[^_]*_", "", basename(drive_credentials)) # Obtain the email
      suppressMessages(
        googledrive::drive_auth(
          email = email,
          cache = ee_path_user
        )
      )

      # This lines is to avoid that users have multiple token file.
      # It delete the older one if the system detect two different token files.
      new_full_credentials <- list.files(path = ee_path_user, full.names = TRUE)
      new_drive_condition <- grepl(".*_.*@.*", basename(new_full_credentials))
      if (sum(new_drive_condition) > 1) {
        files_credentials_time <- file.info(new_full_credentials[new_drive_condition])$ctime
        drive_credential_to_remove <- new_full_credentials[which.min(files_credentials_time)]
        if (!quiet) {
          message("Removing previous Google Drive Token ....")
        }
        file.remove(drive_credential_to_remove)
      }
      break
    }
  }

  # Move credential to the main folder is user is set
  if (!is.null(user)) {
    # Clean previous and copy new GD credentials in ./earthengine folder
    clean_drive <- list.files(ee_path, ".*_.*@.*", full.names = TRUE) %in% list.dirs(ee_path)
    unlink(
      list.files(ee_path, ".*_.*@.*", full.names = TRUE)[!clean_drive]
    )
    file.copy(
      from = drive_credentials,
      to = sprintf("%s/%s", ee_path, basename(drive_credentials)),
      overwrite = TRUE
    )
  }
  invisible(drive_credentials)
}


#' Authorize Google Cloud Storage
#'
#' Authorize Google Cloud Storage to view and manage your gcs files.
#'
#' @details
#' *.json is the authentication file you have downloaded
#' from your Google Project
#' (https://console.cloud.google.com/apis/credentials/serviceaccountkey/).
#' Is necessary to save it (manually) inside the folder ~/.R/earthengine/USER/.
#' @noRd
ee_create_credentials_gcs_ <- function(user, ee_utils) {
  # check packages
  ee_check_packages("ee_Initialize", "googleCloudStorageR")

  # setting gcs folder
  if (is.null(user)) {
    ee_path <- suppressWarnings(ee_utils_py_to_r(ee_utils$ee_path()))
    ee_path_user <- ee_path
  } else {
    ee_path <- suppressWarnings(ee_utils_py_to_r(ee_utils$ee_path()))
    ee_path_user <- sprintf("%s/%s", ee_path, user)
  }

  # gcs_credentials
  full_credentials <- list.files(path = ee_path_user, full.names = TRUE)
  gcs_condition <- grepl(".json", full_credentials)
  if (!any(gcs_condition)) {
    gcs_text <- paste(
      sprintf("Unable to find a service account key (SAK) file in: %s",  crayon::bold(ee_path_user)),
      "Please, download and save the key manually on the path mentioned",
      "before. A tutorial to obtain the SAK file is available at:",
      "> https://r-spatial.github.io/rgee/articles/rgee05.html",
      crayon::bold("As long as you haven't saved a SKA file, the following functions will not work:"),
      "- rgee::ee_gcs_to_local()",
      "- ee_extract(..., via = \"gcs\")",
      "- ee_as_raster(..., via = \"gcs\")",
      "- ee_as_stars(..., via = \"gcs\")",
      "- ee_as_sf(..., via = \"gcs\")",
      "- sf_as_ee(..., via = \"gcs_to_asset\")",
      "- gcs_to_ee_image",
      "- raster_as_ee",
      "- local_to_gcs",
      "- stars_as_ee",
      sep = "\n"
    )
    gcs_info <- list(path = NA, message = gcs_text)
  } else {
    gcs_credentials <- full_credentials[gcs_condition]
    googleCloudStorageR::gcs_auth(gcs_credentials)

    if (!is.null(user)) {
      unlink(list.files(ee_path, ".json", full.names = TRUE))
      file.copy(
        from = gcs_credentials,
        to = sprintf("%s/%s", ee_path, basename(gcs_credentials)),
        overwrite = TRUE
      )
      gcs_info <- list(path = gcs_credentials, message = NA)
    } else {
      gcs_info <- list(path = gcs_credentials, message = NA)
    }
  }
  gcs_info
}


#' Authorize rgee to view and manage your Earth Engine account.
#' This is a three-step function:
#' \itemize {
#' \item First get the full path name of the Earth Engine credentials
#' considering the email address.
#' \item Second, use the file.copy function to set up the
#' "credentials" file, so that the Earth Engine Python API can read it.
#' \item Finally, if the file.copy fails at copy it, the credentials
#' will download from Internet. You will be directed to a web browser.
#' Sign in to your Google account to be granted rgee
#' permission to operate on your behalf with Google Earth Engine.
#' These user credentials are cached in a folder below your
#' home directory, `rgee::ee_get_earthengine_path()`, from
#' where they can be automatically refreshed, as necessary.
#' }
#' @noRd
ee_create_credentials_earthengine <- function(user, auth_quiet, ee_utils, ...) {

  # setting ee folder
  if (is.null(user)) {
    ee_path <- ee_utils_py_to_r(ee_utils$ee_path())
    ee_path_user <- ee_path
  } else {
    ee_path <- ee_utils_py_to_r(ee_utils$ee_path())
    ee_path_user <- sprintf("%s/%s", ee_path, user)
  }

  # Obtain the path of the sub-folder credentials
  user_ee_credential <- sprintf("%s/credentials", ee_path_user)
  main_ee_credential <- sprintf("%s/credentials", ee_path)

  # If the file exist copy in the main folder
  path_condition <- file.exists(user_ee_credential)

  if (isTRUE(path_condition)) {
    if (!is.null(user)) {
      path_condition <- file.copy(
        from = user_ee_credential,
        to = main_ee_credential,
        overwrite = TRUE
      )
    }
  } else {
    # Run authenticate
    extra_params = append(list(...), list(quiet = auth_quiet))
    do.call(ee_Authenticate, extra_params)

    # Copy credentials into the user folder
    Sys.sleep(0.1)
    file.copy(
      from = main_ee_credential,
      to = user_ee_credential
    )
    invisible(TRUE)
  }
}

#' GCS in rgee
#' @noRd
ee_create_credentials_gcs <- function(user, ee_utils) {
  tryCatch(
    expr = ee_create_credentials_gcs_(user, ee_utils),
    error = function(e) {
      list(path = NA, message = NA)
    })
}
