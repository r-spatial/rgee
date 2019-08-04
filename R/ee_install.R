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

#' Install Earth Engine API packages
#'
#' Wrapping to reticulate::py_install(packages = "ee")
#'
#' @author \href{https://github.com/kevinushey}{Kevin Ushey} and \href{https://github.com/jjallaire}{J.J. Allaire}.
#'
#' @param packages Character vector with package names to install.
#' @param envname The name, or full path, of the environment in which Python
#'   packages are to be installed. When `NULL` (the default), the active
#'   environment as set by the `RETICULATE_PYTHON_ENV` variable will be used;
#'   if that is unset, then the `r-reticulate` environment will be used.
#' @param method Installation method. By default, "auto" automatically finds a
#'   method that will work in the local environment. Change the default to force
#'   a specific installation method. Note that the "virtualenv" method is not
#'   available on Windows.
#' @param python_version The requested Python version. Ignored when attempting
#'   to install with a Python virtual environment.
#' @param conda conda Path to conda executable (or "auto" to find conda using
#'  the PATH and other conventional install locations).
#' @param ... Additional arguments passed to [conda_install()]
#'   or [virtualenv_install()].
#'
#' @details On Linux and OS X the "virtualenv" method will be used by default
#'   ("conda" will be used if virtualenv isn't available). On Windows, the
#'   "conda" method is always used.
#'
#' @export
ee_install <- function(packages,
                       envname = NULL,
                       method = c("auto", "virtualenv", "conda"),
                       conda = "auto",
                       python_version = NULL, ...) {
  py_discover_config()
  py_install(packages = "ee",
             envname = envname,
             method = method,
             conda = conda,
             python_version = python_version, ...)
}
