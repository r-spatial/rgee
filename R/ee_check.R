#' Interface to check Python and non-R rgee dependencies
#'
#' R functions for checking sanity of Python Third-Party
#' Python packages and credentials
#' @name ee_check-tools
#' @param quiet logical. Suppress info message
#' @importFrom reticulate py_available py_module_available
#' py_discover_config source_python
#' @examples
#' library(rgee)
#' ee_reattach() # reattach ee as a reserved word
#' ee_check()
#' @export
ee_check <- function() {
  ee_check_python()
  ee_check_rgee_python_packages()
  ee_check_credentials()
}

#' @rdname ee_check-tools
#' @export
ee_check_python <- function(quiet = FALSE) {
  python_test <- py_available(initialize = TRUE)
  if (python_test) {
    py_version <- as.numeric(py_discover_config()$version)
    if (!quiet) {
      cli::cat_line(
        crayon::blue(cli::symbol$circle_filled),
        crayon::blue("  Python version found: "),
        crayon::green(
          py_discover_config()$python,
          sprintf("v%s", py_version)
        )
      )
    }
  } else {
    stop(
      "Unable to find a Python version, you will need to fix it before ",
      "installing rgee. More details through reticulate::py_available()"
    )
  }
  if (py_version < 3.5) stop("rgee just run under Python 3.5 >=")
  return(invisible(TRUE))
}

#' @rdname ee_check-tools
#' @export
ee_check_rgee_python_packages <- function(quiet = FALSE) {
  oauth_func_path <- system.file("python/ee_check_utils_exist.py",
    package = "rgee"
  )
  ee_check_utils_exist <- ee_source_python(oauth_func_path)
  if (isFALSE(quiet)) {
    cli::cat_line(
      "\n",
      crayon::blue(
        cli::symbol$circle_filled,
        " Python Third-Party Libraries used in rgee: \n"
      )
    )
  }
  # ee
  version_ee <- ee_py_to_r(ee_check_utils_exist$ee_check_py_ee())
  ee_cond <- is.character(version_ee)
  if (ee_cond) {
    if (version_ee == ee_version()) {
      if (isFALSE(quiet)) {
        cli::cat_line(
          crayon::green(cli::symbol$tick, "[Ok]"),
          crayon::blue(cli::symbol$check, "Python Earth Engine API version "),
          crayon::green(version_ee)
        )
      }
    } else {
      ee_message <- sprintf(
        "%s (version %s) is %s%s%s%s%s(%s)%s%s%s%s%s%s",
        "The Earth Engine Python API",
        version_ee,
        "installed correctly in the system but rgee was built ",
        "using the version ",
        ee_version(),
        ". To avoid possible issues, we ",
        "highly recommend install the version used by rgee ",
        ee_version(),
        ", you might use:\n >>> ee_install_python_packages() \n",
        " >>> pip install earthengine-api==",ee_version(),
        "\n >>> conda install earthengine-api==",ee_version(),
        "\nIf the installation is successful, restart to see changes."
      )
      warning(ee_message)
    }
  } else {
    if (isFALSE(quiet)) {
      cli::cat_line(
        crayon::red(cli::symbol$tick, "[X]"),
        crayon::red(" Not installed"),
        crayon::red(
          cli::symbol$check,
          "Python Earth Engine API",
          "(earthengine-api)"
        )
      )
    }
  }
  if (isFALSE(quiet)) {
    ee_check_py_package("oauth2client")
  }
}

#' @rdname ee_check-tools
#' @noRd
ee_check_drivers <- function() {
  display_in_browser <- FALSE
  oauth_func_path <- system.file("python/ee_check_utils.py", package = "rgee")
  ee_check_utils <- ee_source_python(oauth_func_path)
  driverdir <- path.expand("~/.config/earthengine")
  condition <- ee_py_to_r(
    ee_check_utils$ee_check_drivers_py(driverdir, display_in_browser)
  )
  cli::cat_line(
    "\n", crayon::blue(cli::symbol$circle_filled),
    crayon::blue("  Selenium drivers: \n")
  )
}

#' @rdname ee_check-tools
#' @export
ee_check_credentials <- function() {
  driverdir <- path.expand("~/.config/earthengine")
  ee_credentials <- sprintf("%s/credentials", driverdir)
  drive_credentials <- list.files(driverdir, "@gmail.com", full.names = TRUE)[1]

  ex_ee_cred <- file.exists(ee_credentials)
  ex_drive_cred <- file.exists(drive_credentials)
  ex_gcs_cred <- file.exists(drive_credentials)

  cli::cat_line(
    crayon::blue(cli::symbol$circle_filled),
    crayon::blue("  Credentials neccesaries for rgee: \n")
  )

  if (ex_ee_cred) {
    cli::cat_line(
      crayon::green(cli::symbol$tick, "[Ok]"),
      crayon::blue(cli::symbol$check, "Earth Engine Credentials found.")
    )
  } else {
    stop(
      "Does not exist Earth Engine credentials in their system.",
      " Try rgee::ee_Initialize() to fixed."
    )
  }

  if (ex_drive_cred) {
    cli::cat_line(
      crayon::green(cli::symbol$tick, "[Ok]"),
      crayon::blue(cli::symbol$check, "Google Drive credentials found.")
    )
  } else {
    cli::cat_line(
      crayon::yellow(
        cli::symbol$circle_cross,
        "Does not exist Google Drive credentials in their",
        "system. rgee::ee_download_drive() will not work."
      )
    )
    message("Try rgee::ee_Initialize(drive = TRUE) to fixed.\n")
  }

  if (ex_gcs_cred) {
    cli::cat_line(
      crayon::green(cli::symbol$tick, "[Ok]"),
      crayon::blue(
        cli::symbol$check,
        "Google Cloud Storage ",
        "credentials found."
      )
    )
  } else {
    cli::cat_line(
      crayon::yellow(
        cli::symbol$circle_cross,
        "Does not exist Google Cloud Storage credentials in their system.",
        'rgee::ee_download_gcs() and rgee::ee_upload(bucket=" ... ") will',
        "not work."
      )
    )
    message("Try rgee::ee_Initialize(gcs = TRUE) to fixed.\n")
  }
}

#' Check python packages
#' @param rgee_package package name to install
#' @noRd
ee_check_py_package <- function(rgee_package) {
  oauth_func_path <- system.file("python/ee_check_utils_exist.py",
    package = "rgee"
  )
  check_py_package <- ee_source_python(oauth_func_path)
  version_rgeepackage <- ee_py_to_r(
    eval(
      parse(
        text = sprintf("check_py_package$ee_check_py_%s()", rgee_package)
      )
    )
  )
  rgeepackage_is_text <- is.character(version_rgeepackage)
  rgeepackage_is_TRUE <- isTRUE(version_rgeepackage)

  if (rgeepackage_is_text) {
    cli::cat_line(
      crayon::green(cli::symbol$tick),
      crayon::green(" [Ok] "),
      crayon::blue(
        rgee_package, "v",
        version_rgeepackage
      )
    )
  }

  if (rgeepackage_is_TRUE) {
    cli::cat_line(
      crayon::green(cli::symbol$tick),
      crayon::green(" [Ok] "),
      crayon::blue(rgee_package)
    )
  }

  if (isFALSE(version_rgeepackage)) {
    stop(
      crayon::bold(rgee_package),
      " has not been found in this Python version, try as follow for fixed: \n",
      sprintf(
        "- rgee::ee_install_python_package('%s', conda = FALSE) \n",
        rgee_package
      ),
      "If the installation is successful, restart to see changes."
    )
  }
}
