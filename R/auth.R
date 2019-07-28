#' Read and evaluate a Python script
#' @noRd
ee_source_python <- function(oauth_func_path) {
  load_test <- try(source_python(file = oauth_func_path,envir = parent.frame()))
  count <- 1
  while (class(load_test) == "try-error" & count < 5) {
    load_test <- try(source_python(file = oauth_func_path),
                     silent = T)
    count <- count + 1
  }
}

#' Initialize the Earth Engine library.
#' @importFrom reticulate source_python
#' @noRd
#'
ee_oauth <- function () {
  oauth_func_path <- system.file("Python/ee_auth.py",package = "rgee")
  ee_source_python(oauth_func_path)

  ee_authenticate()
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

#' Initialize the Earth Engine library.
#'
#' Earth Engine uses the OAuth 2.0 protocol for authenticating clients.
#' Running `ee_initialize` will prompt you through the authentication process
#' using your web browser. See Setting Up Authentication Credentials in the
#' \href{https://developers.google.com/earth-engine/python_install_manual}{Python API documentation}
#' for additional details.
#'
#' @param get_credentials TODO
#'
#' @export
ee_initialize <- function(get_credentials=FALSE) {
  if(get_credentials) {
    ee_oauth()
    ee_init()
  } else {
    oauth_func_path <- system.file("Python/ee_auth.py",package = "rgee")
    ee_source_python(oauth_func_path)
    ee_init()
  }
}


#' Evaluate ee query
#'
#' @param  ... expr
#'
#' @export
ee_evaluate <- function(...) {
  oauth_func_path <- system.file("Python/ee_auth.py",package = "rgee")
  ee_source_python(oauth_func_path)
  total_name <- deparse(substitute(...))
  # TODO -> improve regex
  total_name %>%
    paste(.,collapse = "") %>%
    gsub("\\ee_","\\ee.",.) %>%
    gsub("%>%","\\.",.) %>%
    gsub("\\s","",.) %>%
    py_evaluate(.)
}


#' Return ee query
#'
#' @param  ... expr
#' @export
ee_query <- function(...) {
  oauth_func_path <- system.file("Python/ee_auth.py",package = "rgee")
  ee_source_python(oauth_func_path)
  total_name <- deparse(substitute(...))
  # TODO -> improve regex
  total_name %>%
    paste(.,collapse = "") %>%
    gsub("%>%","\\.",.) %>%
    gsub("\\s","",.)
}

#' Soy una bonita funcion que no hace nada :)
#' @param ID holi
#' @export
ee.ImageCollection <- function(ID) {}
