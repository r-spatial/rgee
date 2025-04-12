#' Interface to check Python and non-R dependencies
#'
#' R function for checking Google credentials (Google Earth Engine,
#' Google Drive and Google Cloud Storage), Python environment and
#' Third-Party Python Packages used by rgee. Besides, from v0.1.304,
#' earthengine-api (Python side) requires gcloud to manage
#' authentication (see \link{ee_Authenticate}).
#' @name ee_check-tools
#'
#' @param user Character.User to check credentials. If this parameter is not defined,
#' then the check for credentials will be skipped.
#' @param quiet Logical. Suppress info message
#'
#' @importFrom reticulate py_available py_module_available py_discover_config
#' source_python
#' @importFrom crayon yellow
#' @importFrom cli cat_line
#' @family ee_check functions
#' @return No return value, called for checking non-R rgee dependencies.
#' @examples
#' \dontrun{
#' library(rgee)
#'
#' ee_check_python()
#' ee_check_python_packages()
#' ee_check_credentials()
#' ee_check_gcloud()
#' ee_check() # put them all together
#'
#' ## Install gcloud in Unix systems
#' ## 1. Download/Install gcloud
#' # system("curl -sSL https://sdk.cloud.google.com | bash")
#' ## 2. Set the PATH ENV
#' # sdkpath <- sprintf("%s/google-cloud-sdk/bin/", Sys.getenv("HOME"))
#' # Sys.setenv(PATH=sprintf("%s:%s", Sys.getenv("PATH"), sdkpath))
#' }
#' @export
ee_check <- function(user = NULL, quiet = FALSE) {
  # Load your Python Session
  # if EARTHENGINE_PYTHON is defined, then send it to RETICULATE_PYTHON
  earthengine_python <- Sys.getenv("EARTHENGINE_PYTHON", unset = NA)
  if (!is.na(earthengine_python))
    Sys.setenv(RETICULATE_PYTHON = earthengine_python)

  ee_check_python(quiet = quiet)
  ee_check_python_packages(quiet = quiet)
  if (!is.null(user)) {
    ee_check_credentials(quiet = quiet)
  }
  # gcloud is need it to authenticate but no for initialize.
  # ee_check_gcloud()

  invisible(TRUE)
}

#' @rdname ee_check-tools
#' @family ee_check functions
#' @export
ee_check_python <- function(quiet = FALSE) {
  python_test <- py_available(initialize = TRUE)
  if (python_test) {
    py_version <- py_discover_config()[["version"]]
    if (!quiet) {
      cat_line(
        blue(symbol[["circle_filled"]]),
        blue("  Python version\n"),
        green(symbol[["tick"]], "[Ok]"),
        blue(
          "",
          py_discover_config()[["python"]],
          sprintf("v%s", py_version)
        )
      )
    }
  } else {
    stop(
      "Unable to find a Python version, you will need to fix before run ",
      "rgee::ee_Initialize(). For more details run reticulate::py_available()"
    )
  }
  
  if (utils::compareVersion(as.character(py_version), "3.5") == -1) {
    stop("rgee needs Python 3.5 >=")
  }
  return(invisible(TRUE))
}

#' @rdname ee_check-tools
#' @family ee_check functions
#' @export
ee_check_python_packages <- function(quiet = FALSE) {
  # Necessary packages
  if (!py_module_available("numpy") & quiet == FALSE) {
    ee_wrong_message("numpy")
    return(invisible(FALSE))
  }
  if (!py_module_available("ee") & quiet == FALSE) {
    ee_wrong_message("earthengine-api")
    return(invisible(FALSE))
  }
  if (!quiet) {
    cat_line(
      blue(symbol[["circle_filled"]]),
      blue("  Python packages:\n"),
      green(symbol[["tick"]], "[Ok]"),
      blue(" numpy\n"),
      green(symbol[["tick"]], "[Ok]"),
      blue(" earthengine-api")
    )
  }
  ee_current_version <- ee[['__version__']]
  if (!ee_current_version == ee_version()) {
    text <- paste(
      sprintf(
        "%s The Earth Engine Python API version %s is installed",
        crayon::bold("NOTE:"),
        ee_current_version
      ),
      "correctly in the system but rgee was tested using the version",
      sprintf(
        "%s. To avoid possible issues, we recommend install the",
        ee_version()
      ),
      sprintf("version used by rgee (%s). You might use:", ee_version()),
      "* rgee::ee_install_upgrade()",
      sprintf(
        "* reticulate::py_install('earthengine-api==%s', envname='PUT_HERE_YOUR_PYENV')",
        ee_version()
      ),
      sprintf(
        "* pip install earthengine-api==%s (Linux and Mac0S)",
        ee_version()
      ),
      sprintf(
        "* conda install earthengine-api==%s (Linux, Mac0S, and Windows)",
        ee_version()
      ),
      "",
      sep = "\n"
    )
    message(text)
  }
  invisible(TRUE)
}

#' @rdname ee_check-tools
#' @family ee_check functions
#' @export
ee_check_credentials <- function(quiet = FALSE) {
  # get the path of earth engine credentials
  oauth_func_path <- system.file("python/ee_utils.py", package = "rgee")
  utils_py <- ee_source_python(oauth_func_path)
  driverdir <- ee_utils_py_to_r(utils_py$ee_path())

  # Exist the EE credential?
  ee_credentials <- sprintf("%s/credentials", driverdir)
  ex_ee_cred <- file.exists(ee_credentials)

  # Exist the googledrive credential?
  drive_credentials <- list.files(driverdir, "@gmail.com", full.names = TRUE)[1]
  ex_drive_cred <- file.exists(drive_credentials)

  # Exist the gcs credential?
  gcs_credentials <- list.files(driverdir, "\\.json$", full.names = TRUE)[1]
  ex_gcs_cred <- file.exists(drive_credentials)

  # Google Earth Engine credentials
  if (!quiet) {
    cat_line(
      blue(symbol[["circle_filled"]]),
      blue("  Credentials neccesaries for rgee:")
    )
  }

  if (ex_ee_cred) {
    if (!quiet) {
      cat_line(
        green(symbol[["tick"]], "[Ok]"),
        blue(symbol[["check"]], "Earth Engine Credentials found.")
      )
    }
  } else {
    stop(
      "Does not exist Earth Engine credentials in their system.",
      " Try rgee::ee_Initialize() to fixed."
    )
  }

  # Google Drive credentials
  if (ex_drive_cred) {
    if (!quiet) {
      cat_line(
        green(symbol[["tick"]], "[Ok]"),
        blue(symbol[["check"]], "Google Drive credentials found.")
      )
    }
  } else {
    if (!quiet) {
      message(
        bold("NOTE: "),
        "This user does not present a Google Drive credential. ",
        "The following functions will not work:\n",
        "- ee_drive_to_local()\n",
        "- ee_as_raster(via = \"drive\")\n",
        "- ee_as_stars(via = \"drive\")\n",
        "- ee_as_sf(via = \"drive\")\n",
        "Try rgee::ee_Initialize(drive = TRUE) to fix.\n"
      )
    }
  }

  # GCS credentials
  if (ex_gcs_cred) {
    if (!quiet) {
      cat_line(
        green(symbol[["tick"]], "[Ok]"),
        blue(
          symbol[["check"]],
          "Google Cloud Storage",
          "credentials found."
        )
      )
    }
  } else {
    if (!quiet) {
      message(
        bold("NOTE: "),
        "This user does not present a GCS credential. ",
        "The following functions will not work:\n",
        "- rgee::ee_gcs_to_local()\n",
        "- ee_as_raster(via = \"gcs\")\n",
        "- ee_as_stars(via = \"gcs\")\n",
        "- ee_as_sf(via = \"gcs\")\n",
        "- sf_as_ee(via = \"gcs_to_asset\")\n",
        "- gcs_to_ee_image\n",
        "- raster_as_ee\n",
        "- local_to_gcs",
        "- stars_as_ee\n",
        "Try rgee::ee_Initialize(gcs = TRUE) to fix.\n"
      )
    }
  }
  invisible(TRUE)
}

#' @rdname ee_check-tools
#' @family ee_check functions
#' @export
ee_check_gcloud <- function() {
  os <- reticulate::import("os")
  return <- os$system(
    "gcloud --help > /dev/null 2>&1"
  )
  if (return != 0) {
    stop(
      "gcloud failed [os.system('gcloud --help')]. Please check
      for any errors above and install gcloud if needed ",
      "(https://cloud.google.com/sdk/docs/install)."
    )
  }
}

#' Wrong message when a Python package is not installed
#' @noRd
ee_wrong_message <- function(packages) {
  cat_line(
    red(symbol[["tick"]], "[X]"),
    red(
      symbol[["check"]],
      bold(packages)
    ),
    red(" not installed")
  )
}
