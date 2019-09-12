#' Authenticate and Initialize rgee
#'
#' Authorize rgee to manage Earth Engine resource The \code{ee_initialize()} via web-browser asked to sign in
#' to your Google account and grant permission for managing google earth engine. This function is a wrapper around
#' `rgee::ee$Initialize()`.
#'
#' @param user_gmail 	Optional. Allows user to target a specific Google identity.
#' @param drive logical. If TRUE drive credential are cached in the system.
#' @param gcs logical. If TRUE drive credential are cached in the system.
#' @param quiet logical. Suppress info messages.
#' @importFrom utils read.table browseURL write.table
#' @importFrom reticulate import_from_path import
#' @details
#' \code{ee_initialize()} give the possibility of authorize Google drive (googledrive) and Google Cloud
#'  Storage (googlecloudStorageR) via `gargle::token_fetch()`. By default, rgee do not need to them, these
#'  are just neccesary for exportation and importation tasks. The user credentials are save in the
#'  folder \code{~/.config/earthengine/USERS/}, if a user is not specified the parameters of the
#'  last session will be used.
#' @examples
#' library(rgee)
#' ee_reattach() # reattach ee as a reserved word
#' ee_Initialize()
#' @export
ee_Initialize <- function(user_gmail, drive = FALSE, gcs = FALSE, quiet = TRUE) {
  #assign("ee",value =  import("ee", delay_load = TRUE), envir = .GlobalEnv)
  ee_path <- path.expand("~/.config/earthengine")
  main_ee_credential <- sprintf("%s/credentials", ee_path)
  session_info <- sprintf("%s/rgee_sessioninfo.txt",ee_path)

  if (!quiet) cat('Requesting Earth Engine authorization  ... ')
  if (missing(user_gmail)) {
    if (file.exists(main_ee_credential)) {
      ee$Initialize()
    } else {
      user <- ee_get_credentials_gee()
      ee$Initialize()
    }
    if (file.exists(session_info)) {
      user <- read.table(file = sprintf("%s/rgee_sessioninfo.txt",ee_path),
                               header = TRUE)[['user']]
    } else {
      user <- gsub("users/","",ee$data$getAssetRoots()[[1]]$id)
    }

    user_gmail <- paste0(user,"@gmail.com")
    user_path <- sprintf("%s/%s/",ee_path, user)

  } else {
    user <- gsub("@gmail.com", "", user_gmail)
    user_path <- sprintf("%s/%s",ee_path, user)
    user_path_EEcredential <- sprintf("%s/credentials",user_path)
    if (file.exists(user_path_EEcredential)) {
      file.copy(from = user_path_EEcredential,
                to = main_ee_credential,
                overwrite = TRUE)
    } else {
      user_ee <- ee_get_credentials_gee()
      if (!identical(user, user_ee)) {
        stop("The Earth Engine Credentials obtained does not correspond with ",
             user_gmail, " but ", sprintf("%s@gmail.com", user_ee))
      }
    }
  }

  if (!quiet) cat('DONE\n')
  user_path_EEcredential <- sprintf("%s/credentials", user_path)
  user_path_GCScredential <- sprintf("%s/GCS_AUTH_FILE.json", user_path)

  if (drive) {
    if (!requireNamespace('googledrive', quietly = TRUE)) {
      stop('The googledrive package is required to use rgee::ee_download_drive',
           call. = FALSE)
    } else {
      if (!quiet) cat('Requesting Google Drive authorization ... ')
      googledrive::drive_auth(email = sprintf("%s@gmail.com",user))
      if (!quiet) cat('DONE\n')
    }
  }

  if (gcs) {
    if (!requireNamespace('googleCloudStorageR', quietly = TRUE)) {
      stop('The googleCloudStorageR package is required to use rgee::ee_download_gcs',
           call. = FALSE)
    } else {
      if (!quiet) cat('Requesting Google Cloud Storage authorization ... ')
      ee_get_credentials_gcs(user_path_GCScredential)
      gcs_auth_tk <- try(googleCloudStorageR::gcs_auth(user_path_GCScredential))
      if (any(class(gcs_auth_tk) %in% "try-error")) {
        stop("Authentication is not possible, check if ",
             user_path_GCScredential,
             " a valid Google Project JSON file")
      }
      if (!quiet) cat('DONE\n')
    }
  }

  ee_sessioninfo(user = user, ee_cre = user_path_EEcredential,
                        gcs_ee = user_path_GCScredential)

  if (!grepl("@gmail.com",user_gmail)) {
    user_gmail <- paste0(user_gmail,"@gmail.com")
  }

  options(rgee.gcs.auth = user_path_GCScredential)
  options(rgee.selenium.params = list(gmail_account = user_gmail,
                                      showpassword = FALSE,
                                      cache = TRUE))
  options(rgee.manage.getAssetAcl = list(writers=user_gmail,
                                         readers=user_gmail,
                                         all_users_can_read = TRUE))
  ee$Initialize()
}

#' Read and evaluate a python script
#' @noRd
ee_source_python <- function(oauth_func_path) {
  module_name <- gsub("\\.py$","",basename(oauth_func_path))
  module_path <- dirname(oauth_func_path)
  ee_module <- import_from_path(module_name, path = module_path, convert = F)
}

#' Authorize rgee
#'
#' Authorize rgee to view and manage your Earth Engine account.
#'
#' By default, you are directed to a web browser, asked to sign in to your Google
#' account, and to grant rgee permission to operate on your behalf with Google Earth
#' Engine. By default, these user credentials are cached in a folder below your home
#' directory, ~/.R/earthengine/USER/credentials, from where they can be automatically
#' refreshed, as necessary.
#' @param save_credentials path to save credentials.
#' @noRd
ee_get_credentials_gee <- function() {
  earthengine_auth <- ee$oauth$get_authorization_url()
  browseURL(earthengine_auth)
  auth_code <- getPass("Enter Earth Engine Authentication: ")
  token <- ee$oauth$request_token(auth_code)
  credential <- sprintf('{"refresh_token":"%s"}',token)
  ee_path <- path.expand("~/.config/earthengine")
  write(credential, sprintf("%s/credentials",ee_path))
  ee$Initialize()
  #Save an create a folder for future inits
  id_user <- ee$data$getAssetRoots()[[1]]$id
  user_name <- gsub("users/","",id_user)
  dir.create(sprintf("%s/%s", ee_path, user_name),showWarnings = FALSE)
  write(credential, sprintf("%s/%s/credentials", ee_path, user_name))
  return(user_name)
}


#' Authorize Google Cloud Storage
#'
#' Authorize Google Cloud Storage to view and manage your gcs files.
#'
#' @param save_credentials Path to save credentials
#'@details
#' GCS_AUTH_FILE.json is the authentication json file you have downloaded from your Google Project
#' (https://console.cloud.google.com/apis/credentials/serviceaccountkey). Is necessary to save it (manually)
#' inside the folder ~/.R/earthengine/USER/.
#' @noRd
ee_get_credentials_gcs <- function(user_path_GCScredential) {
  if (!file.exists(user_path_GCScredential)) {
    message(
      "Unable to find GCS_AUTH_FILE.json in ", user_path_GCScredential,". \n",
      "Firstly download and set the Google Project JSON file in the path mentioned before.\n",
      "A compresible tutorial you can find it here: \n",
      "- https://github.com/csaybar/GCS_AUTH_FILE.json\n",
      "- https://console.cloud.google.com/apis/credentials/serviceaccountkey (download link)\n"
    )
  }
}


#' Create session info inside the folder ~/.config/earthengine/USERS/
#' @noRd
ee_sessioninfo <- function(user = NULL,ee_cre=NULL, gcs_ee = NULL) {
  sessioninfo <- sprintf("%s/rgee_sessioninfo.txt", path.expand("~/.config/earthengine"))
  df <- data.frame(user = user, ee_cre = ee_cre, gcs_ee = gcs_ee,stringsAsFactors = FALSE)
  write.table(df, sessioninfo,row.names = F)
}

