#' Interface to check non-R rgee dependencies
#'
#' R functions for checking sanity of Python Selenium Chromedriver, Third-Party Python packages and credentials
#' @name ee_check-tools
#' @param quiet logical. Suppress info message
#' @importFrom reticulate py_available py_module_available py_discover_config source_python
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_check()
#' }
#' @export
ee_check <- function() {
  ee_check_python()
  ee_check_rgee_python_packages()
  ee_check_drivers()
  ee_check_credentials()
}


#' @rdname ee_check-tools
#' @export
ee_check_python <- function(quiet = FALSE) {
  python_test <- py_available(initialize = TRUE)
  if (python_test) {
    py_version <- as.numeric(py_discover_config()$version)
    if (!quiet) cli::cat_line(crayon::blue(cli::symbol$circle_filled),
                              crayon::blue("  Python version found: "),
                              crayon::green(py_discover_config()$python,sprintf("v%s",py_version)))
  } else {
    stop("Unable to find a Python version, you will need to fix it before installing rgee. ",
         "More details through reticulate::py_available()")
  }
  if (py_version < 3.5) stop("rgee just run under Python 3.5 >=")
  return(invisible(TRUE))
}

#' @rdname ee_check-tools
#' @export
ee_check_rgee_python_packages <- function() {
  oauth_func_path <- system.file("python/ee_check_utils_exist.py", package = "rgee")
  ee_check_utils_exist <- ee_source_python(oauth_func_path)

  cli::cat_line("\n",
                crayon::blue(cli::symbol$circle_filled,
                             " Python Standard Libraries used in rgee: \n"))
  # webbrowser
  ee_check_py_package("webbrowser")
  ee_check_py_package("requests")
  ee_check_py_package("zipfile")
  ee_check_py_package("tarfile")
  ee_check_py_package("subprocess")
  ee_check_py_package("ast")
  ee_check_py_package("sys")
  ee_check_py_package("os")
  ee_check_py_package("platform")
  ee_check_py_package("json")

  cli::cat_line("\n",
                crayon::blue(cli::symbol$circle_filled,
                             " Python Third-Party Libraries used in rgee: \n"))
  # ee
  version_ee <- ee_py_to_r(ee_check_utils_exist$ee_check_py_ee())
  ee_cond <- is.character(version_ee)
  if (ee_cond) {
    if (version_ee == "0.1.175") {
      cli::cat_line(
        crayon::green(cli::symbol$tick, "[Ok]"),
        crayon::blue(cli::symbol$check, "Python Earth Engine API version "),
        crayon::green(version_ee))
    } else {
      ee_message <- sprintf("Earth Engine Python API (ee) %s is installed correctly in their system,%s. %s",
                            version_ee,
                            " but rgee depends on 0.1.175. Please run ee_install_python_ee() for upgrading",
                            "If the installation is successful, restart to see changes.")
      stop(ee_message)
    }
  } else {
    cli::cat_line(crayon::bgRed(cli::symbol$circle_cross, "ee"))
  }
  ee_check_py_package("selenium")
  ee_check_py_package("bs4")
  ee_check_py_package("pysmartDL")
  ee_check_py_package("requests_toolbelt")
}

#' @rdname ee_check-tools
#' @export
ee_check_drivers <- function() {
  display_in_browser = FALSE
  oauth_func_path <- system.file("python/ee_check_utils.py", package = "rgee")
  ee_check_utils <- ee_source_python(oauth_func_path)
  driverdir <- path.expand("~/.config/earthengine")
  condition <- ee_py_to_r(ee_check_utils$ee_check_drivers_py(driverdir, display_in_browser))

  cli::cat_line("\n", crayon::blue(cli::symbol$circle_filled),
           crayon::blue("  Selenium drivers: \n"))
  if (condition) {
    cli::cat_line(
      crayon::green(cli::symbol$tick, "[Ok]"),
      crayon::blue(cli::symbol$check, "Chromedriver \n"))
  } else {
    cli::cat_line(crayon::yellow(cli::symbol$circle_cross,
                            "chromedriver not available in their system.",
                            'rgee::ee_upload(bucket=NULL) will not work.'))
    message("Try rgee::ee_install_drivers() to fixed.\n")
  }
}

#' @rdname ee_check-tools
#' @export
ee_check_credentials <- function() {
  driverdir <- path.expand("~/.config/earthengine")
  ee_credentials <- sprintf("%s/credentials",driverdir)
  drive_credentials <- list.files(driverdir,"@gmail.com",full.names = TRUE)[1]
  gcs_credentials <- sprintf("%s/GCS_AUTH_FILE.json",driverdir)

  ex_ee_cred <- file.exists(ee_credentials)
  ex_drive_cred <- file.exists(drive_credentials)
  ex_gcs_cred <- file.exists(drive_credentials)

  cli::cat_line(crayon::blue(cli::symbol$circle_filled),
           crayon::blue("  Credentials neccesaries for rgee: \n"))

  if (ex_ee_cred) {
    cli::cat_line(
      crayon::green(cli::symbol$tick, "[Ok]"),
      crayon::blue(cli::symbol$check, "Earth Engine Credentials found."))
  } else {
    stop("Does not exist Earth Engine credentials in their system.",
         ' Try rgee::ee_Initialize() to fixed.')
  }

  if (ex_drive_cred) {
    cli::cat_line(
      crayon::green(cli::symbol$tick, "[Ok]"),
      crayon::blue(cli::symbol$check, "Google Drive credentials found."))
  } else {
    cli::cat_line(crayon::yellow(cli::symbol$circle_cross,
                            "Does not exist Google Drive credentials in their system.",
                            'rgee::ee_download_drive() will not work.'))
    message("Try rgee::ee_Initialize(drive = TRUE) to fixed.\n")
  }

  if (ex_gcs_cred) {
    cli::cat_line(
      crayon::green(cli::symbol$tick, "[Ok]"),
      crayon::blue(cli::symbol$check, "Google Cloud Storage credentials found."))
  } else {
    cli::cat_line(crayon::yellow(cli::symbol$circle_cross,
                            "Does not exist Google Cloud Storage credentials in their system.",
                            'rgee::ee_download_gcs() and rgee::ee_upload(bucket=" ... ") will not work.'))
    message("Try rgee::ee_Initialize(gcs = TRUE) to fixed.\n")
  }
}

#' Check python packages
#' @param rgee_package package name to install
#' @noRd
ee_check_py_package <- function(rgee_package) {
  oauth_func_path <- system.file("python/ee_check_utils_exist.py", package = "rgee")
  check_py_package <- ee_source_python(oauth_func_path)
  version_rgeepackage <- ee_py_to_r(eval(parse(text = sprintf("check_py_package$ee_check_py_%s()",
                                                              rgee_package))))
  rgeepackage_is_text <- is.character(version_rgeepackage)
  rgeepackage_is_TRUE <- isTRUE(version_rgeepackage)

  if (rgeepackage_is_text) {
    cli::cat_line(crayon::green(cli::symbol$tick),
             crayon::green(" [Ok] "),
             crayon::blue(rgee_package, "v",
                          version_rgeepackage))
  }

  if (rgeepackage_is_TRUE) {
    cli::cat_line(crayon::green(cli::symbol$tick),crayon::green(" [Ok] "), crayon::blue(rgee_package))
  }

  if ( isFALSE(version_rgeepackage)) {
    stop(crayon::bold(rgee_package),
         " has not been found in this Python version, try as follow for fixed: \n",
         sprintf("- rgee::ee_install_python_package('%s', conda = FALSE) \n",rgee_package),
         "If the installation is successful, restart to see changes.")
  }
}
