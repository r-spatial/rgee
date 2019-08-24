#' Authenticate and Initialize the Earth Engine and Google Drive API
#'
#' @details
#'
#' The \code{ee_initialize()} via web-browser asked to sign in to your Google account and
#' grant permission for both google earth engine and google drive. The user credentials
#' are save in the folder \code{~/.config/earthengine}.
#'
#' @examples
#' \dontrun{
#' ee_initialize()
#' }
#' @export
ee_Initialize <- function() {
  credential_path <-  ee_get_earthengine_path()
  gd_credentials <- sprintf("%s/googledrive", credential_path)
  gee_credentials <- sprintf("%s/credentials", credential_path)

  if (!file.exists(gd_credentials)) ee_get_credentials()
  if (!file.exists(gee_credentials)) ee_get_credentials()

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
