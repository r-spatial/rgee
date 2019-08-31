#' Authenticate and Initialize rgee
#'
#' Authorize rgee to manage Earth Engine accounts. The \code{ee_initialize()} via web-browser asked to sign in
#' to your Google account and grant permission for google earth engine.
#'
#' @param user Earth Engine user
#' @param restart logical. Get credentials again.
#' @param drive logical. If TRUE drive credential are cached in the system.
#' @param gcs logical. If TRUE drive credential are cached in the system.
#' @details
#' \code{ee_initialize()} give the possibility of authorize Google drive and Google Cloud Storage via
#' `gargle::token_fetch()`. By default, rgee do not need to them, these are just neccesary for exportation
#' tasks. The user credentials are save in the folder \code{~/.config/earthengine/USERS/}, if a user is not
#' specified "anonymous" is setting by default.
#' @examples
#' \dontrun{
#' ee_initialize()
#' }
#' @export
ee_Initialize <- function(user,restart = FALSE, drive = FALSE, gcs = FALSE) {
  if (missing(user)) user <- "anonymous@gmail.com"
  if (!grepl("\\@gmail.com$",user)) {
    stop("user needs to be a google account (e.g. anonymous@gmail.com)")
  }
  user <- sub("\\@gmail.com$","",user)
  credential_path <-  sprintf("%s/%s", ee_get_earthengine_path(),user)

  dir.create(credential_path,showWarnings = FALSE)

  gee_credentials <- sprintf("%s/credentials", credential_path)
  gd_credentials <- sprintf("%s/googledrive", credential_path)
  gcs_credentials <- sprintf("%s/GCS_AUTH_FILE.json", credential_path)

  if (restart) {
    ee_get_credentials_gee(credential_path)
    if (drive) ee_get_credentials_drive(credential_path)
    if (gcs) ee_get_credentials_gcs(credential_path)
  } else {
    if (!file.exists(gee_credentials)) ee_get_credentials_gee(credential_path)
    if (!file.exists(gd_credentials) & drive) ee_get_credentials_drive(credential_path)
    if (!file.exists(gcs_credentials) & gcs) ee_get_credentials_gcs(credential_path)
  }
  file.copy(from = gee_credentials,
            to = paste0(ee_get_earthengine_path(),"/credentials"),
            overwrite = TRUE)
  ee$Initialize()
}

#' Read and evaluate a python script
#' @noRd
ee_source_python <- function(oauth_func_path) {
  module_name <- gsub("\\.py$","",basename(oauth_func_path))
  module_path <- dirname(oauth_func_path)
  ee_module <- import_from_path(module_name, path = module_path, convert = F)
  ee_module
}


#' Google drive credential route (DEPRECATED)
#' @noRd
gd_cre_path <- function() {
  sprintf("%s/googledrive", path.expand("~/.config/earthengine"))
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
ee_get_credentials_gee <- function(save_credentials) {
  earthengine_auth <- ee$oauth$get_authorization_url()
  browseURL(earthengine_auth)
  auth_code <- getPass("Enter Earth Engine Authentication: ")
  token <- ee$oauth$request_token(auth_code)
  credential <- sprintf('{"refresh_token":"%s"}',token)
  write(credential, sprintf("%s/credentials",save_credentials))
}

#' Authorize googledrive
#'
#' Authorize googledrive to view and manage your Drive files. This function is
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
#'@details Set the file location of your download Google Project JSON file in
#' a GCS_AUTH_FILE argument (Manually).
#' @noRd
ee_get_credentials_gcs <- function(save_credentials) {
  browseURL("https://console.cloud.google.com/apis/credentials/serviceaccountkey")
  browseURL("https://github.com/csaybar/GCS_AUTH_FILE.json")
}

