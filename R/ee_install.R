#' Check if Python and the Google Earth Engine API are available on this system
#'
#' @param initialize reticulate::py_available params. TRUE to attempt to initialize Python
#' bindings if they aren't yet available (defaults to FALSE).
#' @importFrom reticulate py_available py_module_available
#' @export
#' @examples
#' library(rgee)
#' ee_check()
#' @export
ee_check <- function(initialize=TRUE) {
  python_test <- try(py_available(initialize = initialize), silent = T)
  if (!python_test) {
    stop("No Python version is found")
  }
  cat("The Earth Engine Python API is correctly set up in your system!\n")

  if (!py_module_available("ee")) {
    stop(sprintf("No module named 'ee' found in %s. ",python_info$python),
         "Try running rgee::ee_install()")
  }
 cat("The Earth Engine Python API is correctly set up in your system!\n")
}

#' Install rgee dependecies
#'
#' Wrapping to reticulate::py_install(packages = "ee")
#'
#' @author \href{https://github.com/kevinushey}{Kevin Ushey} and \href{https://github.com/jjallaire}{J.J. Allaire}
#'
#' @param packages Character vector with package names to install.
#' @param method Installation method. By default, "auto" automatically finds a
#'   method that will work in the local environment. Change the default to force
#'   a specific installation method. Note that the "virtualenv" method is not
#'   available on Windows.
#' @param conda conda Path to conda executable (or "auto" to find conda using
#'  the PATH and other conventional install locations).
#'
#' @details On Linux and OS X the "virtualenv" method will be used by default
#'   ("conda" will be used if virtualenv isn't available). On Windows, the
#'   "conda" method is always used.
#'
#' @export
ee_install <- function(packages, method = c("auto", "virtualenv", "conda"), conda = "auto") {
  if(Sys.info()["sysname"]=="Windows"){
    tryCatch({
      system(readLines("inst/install_pip.py"))
      system("pip install earthengine-api")
    })
  }
  if(Sys.info()["sysname"]=="Linux"){
    py_discover_config()
    py_install(packages = "ee",
               method = method,
               conda = conda)
  }
}
