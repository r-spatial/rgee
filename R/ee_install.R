#' Interface to install non-R rgee dependencies
#'
#' R functions to install the necessary third-party python packages
#' for rgee and removing Python Selenium Chromedriver and credentials
#'
#' @param GoogleChromeVersion Google Chrome version of this system.
#' @param user character. User to remove credentials
#' @param method Installation method. By default, "auto" automatically finds a
#' method that will work in the local environment. Change the default to force
#' a specific installation method. Note that the "virtualenv" method is not
#' available on Windows (as this isn't supported by rgee). Note also that
#' since this command runs without privilege the "system" method is
#' available only on Windows.
#' @param conda Path to conda executable (or "auto" to find conda
#' using the PATH and other conventional install locations).
#' @param ee_version earthengine-api version to install. Up to and including
#' earthengine-api 0.1.X, specify "default" to install the 0.1.175 version.
#' @param envname Name of Python environment to install within
#' @param extra_packages Additional Python packages to install along with
#' rgee.
#' @param restart_session Restart R session after installing (note this will
#' only occur within RStudio).
#' @param conda_python_version the python version installed in the created conda
#' environment. Python 3.6 is installed by default.
#' @param ... other arguments passed to [reticulate::conda_install()] or
#' [reticulate::virtualenv_install()].
#' @param quiet logical. Suppress info message
#' @importFrom reticulate source_python py_install
#' @importFrom rstudioapi restartSession hasFun
#' @details It is neccessary restart R to observe change when installing a
#' Python packages. rgee only is compatible with Python version 3.5 >.
#' @name ee_install-tools
#' @examples
#' \dontrun{
#' library(rgee)
#'
#' # Recommended way to use rgee
#' ## 1. Create a virtualenv
#' virtualenv_remove("rgee")
#' virtualenv_create("rgee", python = "python3.7")
#' use_virtualenv("rgee")
#' # rstudioapi::restartSession() # Restart R
#'
#' ## 2. Check dependencies
#' ee_check()
#' ee_install_rgee_python_packages() # Install rgee python packages
#' ee_install_drivers() # Install selenium drivers (see ee_upload)
#' ee_Initialize(drive = TRUE, gcs = TRUE) # Install GCS and DRIVE credentials (optional)
#' ee_check()
#' }
#' @export
ee_install_drivers <- function(GoogleChromeVersion) {
  if (missing(GoogleChromeVersion)) {
    stop(
      "The GoogleChromeVersion argument was not defined.",
      " Find the appropriate version of Google Chrome visiting:\n",
      "- chrome://settings/help \n",
      "- After that run: rgee::ee_install_drivers(77)"
    )
  }

  oauth_func_path <- system.file("python/ee_check_utils.py", package = "rgee")
  ee_check_utils <- ee_source_python(oauth_func_path)
  directory <- path.expand("~/.config/earthengine/")

  os_type <- switch(Sys.info()[["sysname"]],
    Windows = {
      "windows"
    },
    Linux = {
      "linux"
    },
    Darwin = {
      "macos"
    }
  )
  chromedriver_version <- ee_check_utils$download_chromedriver(
    directory = directory,
    operating_system = os_type,
    version = as.character(GoogleChromeVersion)
  )
  cat("Selenium ChromeDriver v", ee_py_to_r(chromedriver_version), "saved in", directory)
  return(invisible(TRUE))
}

#' @rdname ee_install-tools
#' @export
ee_install_rgee_python_packages <- function(method = c("auto", "virtualenv", "conda"),
                                            conda = "auto",
                                            ee_version = "0.1.175",
                                            envname = NULL,
                                            extra_packages = c("selenium", "bs4", "pysmartDL", "requests_toolbelt"),
                                            restart_session = TRUE,
                                            conda_python_version = "3.6",
                                            quiet = quiet,
                                            ...) {
  # verify 64-bit
  if (.Machine$sizeof.pointer != 8) {
    stop(
      "Unable to install rgee on this platform.",
      "Binary installation is only available for 64-bit platforms."
    )
  }

  # verify Python version
  ee_check_python(quiet = TRUE)

  method <- match.arg(method)
  extra_packages <- unique(extra_packages)
  ee_version <- paste0("earthengine-api==", ee_version)

  py_install(
    packages = c(ee_version, extra_packages),
    envname = envname,
    method = method,
    conda = conda,
    python_version = conda_python_version,
    pip = TRUE,
    ...
  )

  cat("\nInstallation complete.\n\n")

  if (restart_session && hasFun("restartSession")) {
    restartSession()
  }

  invisible(NULL)
}



#' @rdname ee_install-tools
#' @export
ee_remove_drivers <- function(quiet = TRUE) {
  ee_path <- path.expand("~/.config/earthengine/")
  gecko_driver_linux <- sprintf("%s/chromedriver", ee_path)
  gecko_driver_win <- sprintf("%s/chromedriver.exe", ee_path)
  if (file.exists(gecko_driver_win)) file.remove(gecko_driver_win)
  if (file.exists(gecko_driver_linux)) file.remove(gecko_driver_linux)
  if (!quiet) cat(sprintf("Credentials in %s has been removed.", ee_path))
}

#' @rdname ee_install-tools
#' @export
ee_remove_credentials <- function(user, quiet = TRUE) {
  ee_path <- path.expand("~/.config/earthengine/")
  ee_credentials <- sprintf("%scredentials", ee_path)
  drive_credentials <- list.files(ee_path, "@gmail.com", full.names = TRUE)[1]
  gcs_credentials <- list.files(ee_path, "GCS_AUTH_FILE.json", full.names = TRUE)[1]
  unlink(paste0(ee_path, user), recursive = TRUE)
  credentials_total <- c(gcs_credentials, drive_credentials, ee_credentials)
  unlink(credentials_total)
  invisible(TRUE)
}
