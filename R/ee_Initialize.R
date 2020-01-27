#' Authenticate and Initialize Earth Engine
#'
#' Authorize rgee to manage Earth Engine resource, Google
#' Drive and Google Cloud Storage. The \code{ee_initialize()} via
#' web-browser asked to sign in to your Google account and
#' grant permission for managing resources. This function is
#' a wrapper around `rgee::ee$Initialize()`.
#'
#' @param email Character (optional, e.g. `pinkiepie@gmail.com`). The email
#' name is used as a folder inside the path `rgee::ee_get_earthengine_path()`.
#' This enable the multi-user support allowing to target a specific
#' Google identity.
#' @param drive Logical (optional). Whether TRUE the drive credential
#' will cache in the path `rgee::ee_get_earthengine_path()`.
#' @param gcs logical. Whether TRUE the Google Cloud Storage
#' credential will cache in the path `rgee::ee_get_earthengine_path()`.
#' @param quiet logical. Suppress info messages.
#' @importFrom utils read.table browseURL write.table packageVersion
#' @importFrom reticulate import_from_path import
#' @import getPass getPass
#' @details
#' \code{ee_Initialize(...)} can also manage Google drive and Google
#' Cloud Storage resources using the R packages googledrive and
#' googlecloudStorageR respectively. By default, rgee do not need to them,
#' these are just necessary for exportation and importation tasks.
#' All the user credentials are save inside the directory
#' \code{~/.config/earthengine/}, if a user does not specified the
#' the email argument all user credentials will be saved in a subdirectory
#' called ndef.
#' @seealso remove credential funtion: \cr
#' \link[rgee]{ee_clean_credentials}
#' @examples
#' # Simple init
#' library(rgee)
#' # ee_reattach() # reattach ee as a reserve word
#' ee_Initialize()
#'
#' # Advanced init
#' expr <- ee_Initialize(
#'   email = "aybar1994@gmail.com",
#'   drive = TRUE,
#'   gcs = TRUE,
#' )
#' @export
ee_Initialize <- function(email = NULL,
                          drive = FALSE,
                          gcs = FALSE,
                          quiet = FALSE) {
  if (!quiet) {
    message(text_col(
      cli::rule(
        left = crayon::bold("rgee", packageVersion("rgee")),
        right = paste0("earthengine-api ", ee_version())
      )
    ))
  }
  # 1. simple checking
  if (is.null(email)) {
    email <- "ndef"
  }

  if (isFALSE(quiet)) {
    if (email == "ndef") {
      cat(
        "", crayon::green(cli::symbol$tick),
        crayon::blue("email:"),
        crayon::green("not_defined\n")
      )
    } else {
      cat(
        "", crayon::green(cli::symbol$tick),
        crayon::blue("email:"),
        crayon::green(email), "\n"
      )
    }
  }

  # create a user's folder
  email_clean <- gsub("@gmail.com", "", email)
  ee_path <- path.expand("~/.config/earthengine")
  ee_path_user <- sprintf("%s/%s", ee_path, email_clean)
  dir.create(ee_path_user, showWarnings = FALSE, recursive = TRUE)

  # Loading all the credentials: earthengine, drive and GCS.
  drive_credentials <- NA
  gcs_credentials <- NA

  if (drive) {
    if (!quiet) {
      cat(
        crayon::green(cli::symbol$tick),
        crayon::blue("Google Drive credentials:")
      )
    }
    drive_credentials <- ee_create_credentials_drive(email)
    if (!quiet) {
      cat(
        "\r",
        crayon::green(cli::symbol$tick),
        crayon::blue("Google Drive credentials:"),
        #drive_credentials,
        crayon::green(" FOUND\n")
      )
    }
  }

  if (gcs) {
    if (!quiet) {
      cat(
        crayon::green(cli::symbol$tick),
        crayon::blue("GCS credentials:")
      )
    }
    gcs_credentials <- ee_create_credentials_gcs(email)
    if (!quiet) {
      cat(
        "\r",
        crayon::green(cli::symbol$tick),
        crayon::blue("GCS credentials:"),
        #gcs_credentials,
        crayon::green(" FOUND\n")
      )
    }
  }

  ## rgee session file
  ee_sessioninfo(
    user = email,
    drive_cre = drive_credentials,
    gcs_cre = gcs_credentials
  )

  options(rgee.gcs.auth = gcs_credentials)
  options(rgee.selenium.params = list(
    email = email,
    user_password = Sys.getenv("PASSWORD_GMAIL"),
    showpassword = FALSE,
    check_driver = FALSE
  ))
  options(rgee.manage.setIamPolicy = list(
    writers = email,
    readers = email,
    all_users_can_read = TRUE
  ))

  if (!quiet) {
    cat(
      "", crayon::green(cli::symbol$tick),
      crayon::blue("Initializing Google Earth Engine:")
    )
  }
  tryCatch(
    expr = ee$Initialize(),
    error = function(e) {
      ee_create_credentials_earthengine(email_clean)
      ee$Initialize()
    }
  )
  if (!quiet) {
    cat(
      "\r",
      crayon::green(cli::symbol$tick),
      crayon::blue("Initializing Google Earth Engine:"),
      crayon::green(" DONE!\n")
    )
  }
  if (!quiet) {
    message(text_col(
      cli::rule(
        #center = crayon::bold("Welcome to Earth Engine <3")
      )
    ))
  }
  invisible(TRUE)
}


#' Authorize rgee to view and manage your Earth Engine account.
#' This is a three-step function:
#' \itemize {
#' \item Firstly will get the full path name of the EE credentials
#' considering the email argument.
#' \item Secondly, using the file.copy function will try to set up the
#' credentials, so that the Earth Engine Python API can read it.
#' \item Finally, if the file.copy do not have success the credentials
#' will download from Internet. By default, you are directed to a web
#' browser, asked to sign in to your Google account, and to grant rgee
#' permission to operate on your behalf with Google Earth Engine.
#' These user credentials are cached in a folder below your
#' home directory, `rgee::ee_get_earthengine_path()`, from
#' where they can be automatically refreshed, as necessary.
#' }
#' @noRd
#'
ee_create_credentials_earthengine <- function(email_clean) {
  # first step
  ee_path <- path.expand("~/.config/earthengine")
  main_ee_credential <- sprintf("%s/credentials", ee_path)
  user_ee_credential <- sprintf(
    "%s/%s/credentials",
    ee_path,
    email_clean
  )
  # second step
  path_condition <- file.exists(user_ee_credential)
  if (path_condition) {
    path_condition <- file.copy(
      from = user_ee_credential,
      to = main_ee_credential,
      overwrite = TRUE
    )
  }
  # third step
  if (!path_condition) {
    earthengine_auth <- ee$oauth$get_authorization_url()
    browseURL(earthengine_auth)
    auth_code <- getPass("Enter Earth Engine Authentication: ")
    token <- ee$oauth$request_token(auth_code)
    credential <- sprintf('{"refresh_token":"%s"}', token)
    write(credential, main_ee_credential)
    write(credential, user_ee_credential)
  }
}

#' Create credentials Google Drive
#' @noRd
ee_create_credentials_drive <- function(email) {
  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop("The googledrive package is not installed. ",
      'Try install.packages("googledrive")',
      call. = FALSE
    )
  }
  # setting drive folder
  ee_path <- path.expand("~/.config/earthengine")
  email_clean <- gsub("@gmail.com", "", email)
  ee_path_user <- sprintf("%s/%s", ee_path, email_clean)
  # drive_credentials
  repeat {
    full_credentials <- list.files(path = ee_path_user, full.names = TRUE)
    drive_condition <- grepl("@gmail.com", full_credentials)
    if (!any(drive_condition)) {
      suppressMessages(
        googledrive::drive_auth(
          email = NULL,
          cache = ee_path_user
        )
      )
    } else {
      drive_credentials <- full_credentials[drive_condition]
      email <- sub("^[^_]*_", "", drive_credentials)
      suppressMessages(
        googledrive::drive_auth(
          email = email,
          cache = ee_path_user
        )
      )
      break
    }
  }
  # from user folder to EE folder
  unlink(list.files(ee_path, "@gmail.com", full.names = TRUE))
  file.copy(
    from = drive_credentials,
    to = sprintf("%s/%s", ee_path, basename(drive_credentials)),
    overwrite = TRUE
  )
  invisible(drive_credentials)
}


#' Authorize Google Cloud Storage
#'
#' Authorize Google Cloud Storage to view and manage your gcs files.
#'
#' @details
#' *.json is the authentication json file you have downloaded
#' from your Google Project
#' (https://console.cloud.google.com/apis/credentials/serviceaccountkey).
#' Is necessary to save it (manually) inside the folder ~/.R/earthengine/USER/.
#' @noRd
ee_create_credentials_gcs <- function(email) {
  if (!requireNamespace("googleCloudStorageR", quietly = TRUE)) {
    stop("The googleCloudStorageR package is not installed. Try",
      ' install.packages("googleCloudStorageR")',
      call. = FALSE
    )
  }
  # setting gcs folder
  ee_path <- path.expand("~/.config/earthengine")
  email_clean <- gsub("@gmail.com", "", email)
  ee_path_user <- sprintf("%s/%s", ee_path, email_clean)
  # gcs_credentials
  full_credentials <- list.files(path = ee_path_user, full.names = TRUE)
  gcs_condition <- grepl(".json", full_credentials)
  if (!any(gcs_condition)) {
    stop(
      "Unable to find GCS_AUTH_FILE.json in ", ee_path_user, ". \n",
      "Firstly download and save the Google Project JSON file in ",
      "the path mentioned before.\n",
      "A compressible tutorial for obtaining GCS_AUTH_FILE.json is ",
      "available here: \n",
      "- https://github.com/csaybar/GCS_AUTH_FILE.json\n",
      "- https://console.cloud.google.com/apis/credentials/serviceaccountkey ",
      "(download link)\n"
    )
  } else {
    gcs_credentials <- full_credentials[gcs_condition]
    googleCloudStorageR::gcs_auth(gcs_credentials)
  }
  unlink(list.files(ee_path, ".json", full.names = TRUE))
  file.copy(
    from = gcs_credentials,
    to = sprintf("%s/%s", ee_path, basename(gcs_credentials)),
    overwrite = TRUE
  )
  invisible(gcs_credentials)
}

#' Create session info of the last init inside the
#' folder ~/.config/earthengine/
#' @noRd
ee_sessioninfo <- function(user = NULL, drive_cre = NULL, gcs_cre = NULL) {
  sessioninfo <- sprintf(
    "%s/rgee_sessioninfo.txt",
    path.expand("~/.config/earthengine")
  )
  df <- data.frame(
    user = user, drive_cre = drive_cre, gcs_cre = gcs_cre,
    stringsAsFactors = FALSE
  )
  write.table(df, sessioninfo, row.names = F)
}


#' Get the path where rgee save the credentials
#' @export
ee_get_earthengine_path <- function() {
  ee_path <- path.expand("~/.config/earthengine")
  sessioninfo <- sprintf(
    "%s/rgee_sessioninfo.txt",
    path.expand("~/.config/earthengine")
  )
  if (file.exists(sessioninfo)) {
    user <- read.table(sessioninfo,
      header = TRUE,
      stringsAsFactors = FALSE
    )[[1]]
    if (is.na(user)) {
      stop("rgee_sessioninfo.txt malformed")
    }
  } else {
    stop(
      "rgee_sessioninfo.txt does not exist, ",
      "run rgee::ee_Initialize) to fixed."
    )
  }
  return(sprintf("%s/%s/", ee_path, user))
}

#' Read and evaluate a python script
#' @noRd
ee_source_python <- function(oauth_func_path) {
  module_name <- gsub("\\.py$", "", basename(oauth_func_path))
  module_path <- dirname(oauth_func_path)
  import_from_path(module_name, path = module_path, convert = F)
}


#' function obtained from tidyverse
#' https://github.com/tidyverse/tidyverse
#' @noRd
text_col <- function(x) {
  # If RStudio not available, messages already printed in black
  if (!rstudioapi::isAvailable()) {
    return(x)
  }
  if (!rstudioapi::hasFun("getThemeInfo")) {
    return(x)
  }
  theme <- rstudioapi::getThemeInfo()
  if (isTRUE(theme$dark)) crayon::white(x) else crayon::black(x)
}

#' Earth Engine API version
#'
#' This function return the Earth Engine Python API
#' version with which rgee was built
#' @noRd
ee_version <- function() {
  "0.1.210"
}
