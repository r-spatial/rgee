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

#' Initialize the Earth Engine library.
#' @importFrom reticulate source_python
#' @importFrom googledrive drive_auth
#' @noRd
#'
ee_oauth <- function() {
  oauth_func_path <- system.file("Python/ee_auth.py", package = "rgee")
  ee_source_python(oauth_func_path)

  ee_authenticate()
  drive_auth()
  code <- readline("Enter authorisation code for Earth Engine API here: ")
  test <- try(request_ee_token(code), silent = T)

  while (class(test) == "try-error") {
    cat("Problem with Authentication key input. \nPlease follow the authentication steps in the browser and copy paste the authentication token into the R console again.")
    ee_authenticate()
    code <- readline("enter authorisation code here: ")
    test <- try(request_ee_token(code), silent = T)
  }
  cat("Earth Engine Python API is authenticated \n")
}


#' Authenticate and Initialize the Earth Engine library.
#'
#' R wrapper for ee.Initialize(): Authenticate and Initialize the Earth Engine API. See details.
#'
#'
#' @details
#'
#' The \code{ee_initialize()} will allow you, via web-browser, obtain the
#' \href{https://developers.google.com/earth-engine/python_install_manual#setting-up-authentication-credentials}{OAuth 2.0 Client Credential}
#' . It is necessary for getting access to Google Earth Engine. Copy and
#' paste the key into the R console when prompted for the key.
#'
#' @param get_credentials TODO
#' @examples
#' \dontrun{
#' ee_initialize()
#' }
#' @export
#'
ee_initialize <- function(get_credentials = FALSE) {
  if (get_credentials) {
    ee_oauth()
    ee_init()
  } else {
    oauth_func_path <- system.file("Python/ee_auth.py", package = "rgee")
    ee_source_python(oauth_func_path)
    ee_init()
  }
}
