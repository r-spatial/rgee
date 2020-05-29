#' Interface to check Python and non-R dependencies
#'
#' R functions for checking Google credentials (Google Earth Engine,
#' Google Drive and Google Cloud Storage) available in their system and the
#' Python environment and the Third-Party Python Packages used by rgee.
#'
#' @name ee_check-tools
#'
#' @param quiet Logical. Suppress info message
#'
#' @importFrom reticulate py_available py_module_available py_discover_config
#' source_python
#' @importFrom crayon yellow
#' @importFrom cli cat_line
#' @family ee_check functions
#' @examples
#' \donttest{
#' library(rgee)
#'
#' ee_reattach() # reattach ee as a reserved word
#'
#' ee_check_python()
#' ee_check_python_packages()
#' ee_check_credentials()
#' ee_check() # put it all together
#' }
#' @export
ee_check <- function() {
  ee_check_python()
  ee_check_python_packages()
  ee_check_credentials()
}

#' @rdname ee_check-tools
#' @family ee_check functions
#' @export
ee_check_python <- function(quiet = FALSE) {
  python_test <- py_available(initialize = TRUE)
  if (python_test) {
    py_version <- as.numeric(py_discover_config()$version)
    if (!quiet) {
      cat_line(
        blue(symbol$circle_filled),
        blue("  Python version found: "),
        green(
          py_discover_config()$python,
          sprintf("v%s", py_version)
        )
      )
    }
  } else {
    stop(
      "Unable to find a Python version, you will need to fix it before run ",
      "rgee::ee_Initialize(). For more details run reticulate::py_available()"
    )
  }
  if (py_version < 3.5) stop("rgee needs Python 3.5 >=")
  return(invisible(TRUE))
}

#' @rdname ee_check-tools
#' @family ee_check functions
#' @export
ee_check_python_packages <- function(quiet = FALSE) {
  oauth_func_path <- system.file("python/ee_check.py",
    package = "rgee"
  )
  ee_check_utils_exist <- ee_source_python(oauth_func_path)
  if (!quiet) {
    cat_line(
      "\n",
      blue(
        symbol$circle_filled,
        " Python Third-Party Libraries used in rgee: \n"
      )
    )
  }
  # ee
  version_ee <- ee_utils_py_to_r(ee_check_utils_exist$ee_check_py_ee())
  ee_cond <- is.character(version_ee)
  if (ee_cond) {
    if (version_ee == ee_version()) {
      if (!quiet) {
        cat_line(
          green(symbol$tick, "[Ok]"),
          blue(symbol$check, "Python Earth Engine API version "),
          green(version_ee)
        )
      }
    } else {
      message(text)
      text <- paste(
        sprintf(
          "%s The Earth Engine Python API version %s is installed",
          crayon::bold("NOTE:"),
          version_ee
        ),
        "correctly in the system but rgee was tested using the version",
        sprintf(
          "%s. To avoid possible issues, we recommend install the",
          ee_version()
        ),
        sprintf("version used by rgee (%s). You might use:", ee_version()),
        "* ee_install()",
        sprintf(
          "* reticulate::py_install('earthengine-api==%s')",
          ee_version()
        ),
        sprintf(
          "* pip install earthengine-api==%s (Linux and Mac0S)",
          ee_version()
        ),
        sprintf(
          "* conda install earthengine-api==%s (Windows)",
          ee_version()
        ),
        "",
        sep = "\n"
      )
      message(text)
    }
  } else {
    if (isFALSE(quiet)) {
      cat_line(
        red(symbol$tick, "[X]"),
        red(" Not installed"),
        red(
          symbol$check,
          "Python Earth Engine API",
          "(earthengine-api)"
        )
      )
    }
  }
  if (!quiet) {
    ee_check_py_package("pyasn1")
    ee_check_py_package("urllib3")
    ee_check_py_package("setuptools")
    ee_check_py_package("oauth2client")
    ee_check_py_package("numpy")
    cat("\n")
  }
}

#' @rdname ee_check-tools
#' @family ee_check functions
#' @export
ee_check_credentials <- function(quiet = FALSE) {
  oauth_func_path <- system.file("python/ee_utils.py", package = "rgee")
  utils_py <- ee_source_python(oauth_func_path)
  driverdir <- ee_utils_py_to_r(utils_py$ee_path())
  ee_credentials <- sprintf("%s/credentials", driverdir)
  drive_credentials <- list.files(driverdir, "@gmail.com", full.names = TRUE)[1]

  ex_ee_cred <- file.exists(ee_credentials)
  ex_drive_cred <- file.exists(drive_credentials)
  ex_gcs_cred <- file.exists(drive_credentials)
  if (!quiet) {
    cat_line(
      blue(symbol$circle_filled),
      blue("  Credentials neccesaries for rgee: \n")
    )
  }

  if (ex_ee_cred) {
    if (!quiet) {
      cat_line(
        green(symbol$tick, "[Ok]"),
        blue(symbol$check, "Earth Engine Credentials found.")
      )
    }
  } else {
    stop(
      "Does not exist Earth Engine credentials in their system.",
      " Try rgee::ee_Initialize() to fixed."
    )
  }

  if (ex_drive_cred) {
    if (!quiet) {
      cat_line(
        green(symbol$tick, "[Ok]"),
        blue(symbol$check, "Google Drive credentials found.")
      )
    }
  } else {
    if (!quiet) {
      cat_line(
        yellow(
          symbol$circle_cross,
          "Does not exist Google Drive credentials in their",
          "system. rgee::ee_download_drive() will not work."
        )
      )
      message("Try rgee::ee_Initialize(drive = TRUE) to fixed.\n")
    }
  }

  if (ex_gcs_cred) {
    if (!quiet) {
      cat_line(
        green(symbol$tick, "[Ok]"),
        blue(
          symbol$check,
          "Google Cloud Storage ",
          "credentials found."
        )
      )
    }
  } else {
    if (!quiet) {
      cat_line(
        yellow(
          symbol$circle_cross,
          "Does not exist Google Cloud Storage credentials in their system.",
          'rgee::ee_download_gcs() and rgee::ee_upload(bucket=" ... ") will',
          "not work."
        )
      )
      message("Try rgee::ee_Initialize(gcs = TRUE) to fixed.\n")
    }
  }
  invisible(TRUE)
}

#' Check python packages
#' @param rgee_package package name to install
#' @noRd
ee_check_py_package <- function(rgee_package, quiet = FALSE) {
  oauth_func_path <- system.file("python/ee_check.py",
    package = "rgee"
  )
  check_py_package <- ee_source_python(oauth_func_path)
  version_rgeepackage <- ee_utils_py_to_r(
    eval(
      parse(
        text = sprintf("check_py_package$ee_check_py_%s()", rgee_package)
      )
    )
  )
  rgeepackage_is_text <- is.character(version_rgeepackage)
  rgeepackage_is_TRUE <- isTRUE(version_rgeepackage)

  if (rgeepackage_is_text) {
    if (!quiet) {
      cat_line(
        green(symbol$tick),
        green(" [Ok] "),
        blue(
          rgee_package, "v",
          version_rgeepackage
        )
      )
    }
  }

  if (rgeepackage_is_TRUE) {
    if (!quiet) {
      cat_line(
        green(symbol$tick),
        green(" [Ok] "),
        blue(rgee_package)
      )
    }
  }

  if (isFALSE(version_rgeepackage)) {
    stop(
      bold(rgee_package),
      " has not been found in this Python version, try as follow for fixed: \n",
      sprintf(
        "- reticulate::py_install('%s') \n",
        rgee_package
      ),
      "If the installation is successful, restart to see changes."
    )
  }
  invisible(TRUE)
}
