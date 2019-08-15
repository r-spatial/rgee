#' Check if Python and the Google Earth Engine API are available on this system
#'
#' @param initialize reticulate::py_available params. TRUE to attempt to initialize Python
#' bindings if they aren't yet available (defaults to FALSE).
#' @importFrom reticulate py_available py_module_available
#' @export
#' @examples
#' \dontrun{
#' # Simple install
#' library(rgee)
#' ee_check()
#' }
#' @export
ee_check <- function(initialize=TRUE) {
  python_test <- try(py_available(initialize = initialize), silent = T)
  if (!python_test) {
    stop("No Python version is found")
  }
  cat(" Python is correctly set up in your system!\n")
  pydiscv <- py_discover_config()$python
  if (!py_module_available("ee")) {
    stop(sprintf("No module named 'ee' found in %s. ",pydiscv),
         "Try running rgee::ee_install() and reboot")
  }
 cat(" The Earth Engine Python API is correctly set up in your system!\n")
}

#' Install rgee dependecies
#'
#' Wrapping to reticulate::py_install(packages = "ee")
#'
#' @author \href{https://github.com/kevinushey}{Kevin Ushey} and \href{https://github.com/jjallaire}{J.J. Allaire}
#'
#' @param python_version The requested Python version.
#' @param conda whether TRUE will use conda instead pip.
#' @examples
#' \dontrun{
#' # Simple install
#' library(rgee)
#' ee_install()
#'
#' # Install Earth Engine Python API in a virtualenv
#' library(reticulate)
#' library(rgee)
#' py_discover_config()
#' virtualenv_create("rgee", python = "python2.7")
#' use_virtualenv("rgee")
#' py_discover_config()
#' ee_install()
#' }
#' @export
ee_install <- function(python_version, conda=FALSE) {
  if (missing(python_version)) {
      pydiscv <- py_discover_config()
      python_version <- pydiscv$version
  }
  if (conda) {
    msg_return <- suppressWarnings(system2("conda"))
    if (msg_return!=0) {
      stop("conda is not installed in the system, try using pip ee_install(conda=FALSE)")
    } else {
      system("conda install earthengine-api")
    }
  } else{
    msg_return <- system2(sprintf("python%s",python_version)," -m pip --version")
    if (msg_return!=0) {
      stop("pip is not installed in your system, try using conda ee_install(conda=TRUE)")
    } else {
      install <- suppressWarnings(system2(sprintf("pip%s",python_version),"install earthengine-api"))
      if (install !=0) {
        python_version <- substring(python_version,1,1)
        install <- system2(sprintf("pip%s",python_version),"install earthengine-api")
        if (install !=0) {
          stop("An unexpected error occurred when try to use pip")
        }
      }
    }
  }
}
