#' Authenticate and Initialize rgee
#'
#' Authorize rgee to manage Earth Engine accounts. The \code{ee_initialize()} via web-browser asked to sign in
#' to your Google account and grant permission for managing google earth engine. This function is a wrapper around
#' `rgee::ee$Initialize()`.
#'
#' @param user Earth Engine user
#' @param restart logical. Get credentials again.
#' @param drive logical. If TRUE drive credential are cached in the system.
#' @param gcs logical. If TRUE drive credential are cached in the system.
#' @details
#' \code{ee_initialize()} give the possibility of authorize Google drive and Google Cloud Storage via
#' `gargle::token_fetch()`. By default, rgee do not need to them, these are just neccesary for exportation
#' tasks. The user credentials are save in the folder \code{~/.config/earthengine/USERS/}, if a user is not
#' specified the user parameters of the last session will be used.
#' @examples
#' ee_initialize()
#' @export
ee_Initialize <- function(user,restart = FALSE, drive = FALSE, gcs = FALSE) {
  ee_path <- path.expand("~/.config/earthengine")
  main_ee_credential <- sprintf("%s/credentials", ee_path)

  if (restart) {
    all_files <- list.files(ee_path,recursive = TRUE, include.dirs = TRUE, full.names = TRUE)
    unlink(all_files)
  }

  if (!file.exists(main_ee_credential) | !missing(user)) {
    if (!missing(user)) {
      if (!grepl("\\@gmail.com$",user)) {
        stop("user needs to be a google account (e.g. anonymous@gmail.com)")
      }
      clean_user <- sub("\\@gmail.com$","",user)
      user_ee_credential <- sprintf("%s/%s/credentials",ee_path, clean_user)
      iscopy = file.copy(from = user_ee_credential,
                         to = main_ee_credential,
                         overwrite = TRUE)
      if (!iscopy) {
        ee_get_credentials_gee()
        clean_user = gsub("users/","",ee$data$getAssetRoots()[[1]]$id)
        n_user <- paste0(clean_user,"@gmail.com")
        if (!identical(user,n_user)) {
          stop("Credentials obtained does not correspond with ",
               user, " but ", n_user)
          unlink(paste0(ee_path,"/",gsub("@gmail.com","",user)))
        }
      }
    } else {
      ee_get_credentials_gee()
    }
  } else {
    ee$Initialize()
    clean_user = gsub("users/","",ee$data$getAssetRoots()[[1]]$id)
    user <- paste0(clean_user,"@gmail.com")
    user_ee_credential <- sprintf("%s/%s/credentials",ee_path, clean_user)
  }

  gee_credentials <- sprintf("%s/credentials", user_ee_credential)
  gd_credentials <- sprintf("%s/googledrive", user_ee_credential)
  gcs_credentials <- sprintf("%s/GCS_AUTH_FILE.json", user_ee_credential)

  if (!file.exists(gd_credentials) & drive) ee_get_credentials_drive(credential_path)
  if (!file.exists(gcs_credentials) & gcs) ee_get_credentials_gcs(credential_path)

  ee_sessioninfo(clean_user,gee_credentials,gd_credentials,gcs_credentials)

  options(rgee.gcs.auth = gcs_credentials)
  options(rgee.selenium.params = list(gmail_account = user,
                                      showpassword = FALSE,
                                      cache = TRUE))
  options(rgee.manage.getAssetAcl = list(writers=user,
                                         readers=user,
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
}
#' Authorize googledrive
#'
#' Authorize googledrive to view and manage your drive files. This function is
#' a wrapper around `googledrive::drive_auth()`.
#'
#' By default, you are directed to a web browser, asked to sign in to your Google
#' account, and to grant rgee permission to operate on your behalf with Google Drive.
#' By default, these user credentials are cached in a folder below your home directory,
#' ~/.R/earthengine/USER/googledrive, from where they can be automatically refreshed,
#' as necessary. Storage at the user level means the same token can be used across
#' multiple projects and tokens are less likely to be synced to the cloud by accident.
#'
#' @param save_credentials Path to save credentials
#' @noRd
ee_get_credentials_drive <- function(save_credentials) {
  if (!requireNamespace('googledrive', quietly = TRUE)) {
    stop('The googledrive package is required to use rgee::ee_download_drive',
         call. = FALSE)
  } else {
    gd_credentials <- sprintf("%s/googledrive", save_credentials)
    saveRDS(
      googledrive::drive_auth(cache = F),
      gd_credentials
    )
  }
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
ee_get_credentials_gcs <- function(save_credentials) {
  message(
    "Unable to find GCS_AUTH_FILE.json in ", ee_get_earthengine_path(),". ",
    "Firstly download and set correctly the Google Project JSON file.",
    "A compresible tutorial you can find it here: \n",
    "- https://github.com/csaybar/GCS_AUTH_FILE.json\n",
    "- https://console.cloud.google.com/apis/credentials/serviceaccountkey (download link)\n"
  )
}

#' Create session info inside the folder \code{~/.config/earthengine/USERS/}
#' @noRd
ee_sessioninfo <- function(user = NULL,ee_cre=NULL,drive_cre=NULL, gcs_ee = NULL) {
  sessioninfo <- sprintf("%s/rgee_sessioninfo.txt", path.expand("~/.config/earthengine"))
  df <- data.frame(user = user, ee_cre = ee_cre, drive_cre =  drive_cre,
                   gcs_ee = gcs_ee)
  write.table(df, sessioninfo,row.names = F)
}
