#' Read and evaluate a Python script
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

#' Authorize Earth Engine and Google Drive API
#' @author Cesar Aybar getting from JesJehle <https://github.com/JesJehle/earthEngineGrabR>
#' @importFrom reticulate source_python
#' @importFrom googledrive drive_auth
#' @details Credentials will be saved into path.expand("~/.config/earthengine")
#' @noRd
ee_oauth <- function() {
  oauth_func_path <- system.file("Python/ee_auth.py", package = "rgee")
  ee_source_python(oauth_func_path)

  ee_authenticate_py()
  code <- readline("Enter authorisation code for Earth Engine API here: ")
  test <- try(request_ee_token_py(code), silent = T)
  saveRDS(
    drive_auth(reset = T, cache = F, verbose = F),
    gd_cre_path()
  )

  while (class(test) == "try-error") {
    cat("Problem with Authentication key input. \n
        Please follow the authentication steps in the browser and copy
        paste the authentication token into the R console again.")
    ee_authenticate_py()
    code <- readline("enter authorisation code here: ")
    test <- try(request_ee_token_py(code), silent = T)
  }
}

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
ee_initialize <- function() {
  ee_check()
  credential_path <- path.expand("~/.config/earthengine")
  gd <- sprintf("%s/googledrive", credential_path)
  gee <- sprintf("%s/credentials", credential_path)
  oauth_func_path <- system.file("Python/ee_auth.py", package = "rgee")
  ee_source_python(oauth_func_path)

  credentials_exist <- file.exists(gd) && file.exists(gee)
  if (!credentials_exist) ee_oauth() # Authorize Earth Engine and Google Drive
  drive_auth(gd_cre_path())
  ee_init_py()
  cat(
    "",
    "Earth Engine Python API is authenticated\n",
    "Google Drive API is authenticated"
  )
}


#' Remove the Earth Engine and Google Drive credentials in this system
#'
#' @export
ee_remove_credentials <- function(path,quiet = TRUE) {
  if (missing(path)) path <- dirname(rgee:::gd_cre_path())
  ee_credentials <- sprintf("%s/credentials",path)
  if (file.exists(rgee:::gd_cre_path())) file.remove(rgee:::gd_cre_path())
  if (file.exists(ee_credentials)) file.remove(ee_credentials)
  if (!quiet) cat(sprintf("Credentials in %s has been removed.",path))
}

#' Google drive credential route
#' @noRd
gd_cre_path <- function() {
  sprintf("%s/googledrive", path.expand("~/.config/earthengine"))
}

