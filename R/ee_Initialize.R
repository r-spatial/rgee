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
#' @author JesJehle <https://github.com/JesJehle/earthEngineGrabR>
#' @noRd
ee_source_python <- function(oauth_func_path) {
  load_test <- try(source_python(file = oauth_func_path, envir = parent.frame()))
  count <- 1
  while (class(load_test) == "try-error" & count < 5) {
    load_test <- try(source_python(file = oauth_func_path),
                     silent = T
    )
    count <- count + 1
  }
}


#' Google drive credential route (DEPRECATED)
#' @noRd
gd_cre_path <- function() {
  sprintf("%s/googledrive", path.expand("~/.config/earthengine"))
}
