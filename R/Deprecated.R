#' Discover all the Python environments available in the system (DEPRECATED)
#'
#' This function enables callers to check which versions
#' of Python will be discovered on a system.
#'
#' @param use_py_discover_config Logical. If TRUE
#' will use \link[=reticulate]{py_discover_config} to find
#' versions of Python in the system.  Otherwise, will use
#' \link[=reticulate]{conda_list} for Window OS and
#' \link[=reticulate]{virtualenv_list} for Unix system.
#' @importFrom reticulate py_discover_config conda_list
#' @return Python configuration object (reticulate).
#' @examples
#' \dontrun{
#' library(rgee)
#'
#' ## It is necessary just once, not mandatory
#'
#' # 1. Create a Python environment, e.g. ee.
#' pyenv <- ee_install_create_pyenv(py_env = "ee")
#'
#' # OPTIONAL: Find others Python path in the system.
#' # ee_install_discover_pyenvs()
#'
#' # 2. Set a Python path in .Renviron (EARTHENGINE_PYTHON)
#' # to be used in future sessions
#' ee_install_set_pyenv(pyenv)
#'
#' # 3. Now run ee_Initialize()
#' ee_Initialize()
#' }
#' @export
ee_install_discover_pyenvs <- function(use_py_discover_config = TRUE) {
  .Deprecated(
    msg = paste0(
      "Due to recent changes in reticulate it is no longer necessary.",
      " ee_install_discover_pyenvs will be removed in rgee 0.5.3"
    ),
  )
  if (is_windows()) {
    if (isTRUE(use_py_discover_config)) {
      print(py_discover_config())
      ret_info <- py_discover_config()
      if (!is.null(ret_info$forced)) {
        cat(
          "NOTE: Python version was forced by",
          ret_info$forced,
          "consider remove this system variable",
          "to display more options.",
          "\n"
        )
      }
      invisible(ret_info$python_versions)
    } else {
      conda_list()$python
    }
  } else {
    if (isTRUE(use_py_discover_config)) {
      print(py_discover_config())
      ret_info <- py_discover_config()
      if (!is.null(ret_info$forced)) {
        cat(
          "NOTE: Python version was forced by",
          ret_info$forced,
          "consider remove this system variable",
          "to display more options.",
          "\n"
        )
      }
      invisible(ret_info$python_versions)
    } else {
      ee_virtualenv_list()
    }
  }
}

#' Install rgee Python packages dependencies (DEPRECATED)
#'
#' Install the necessary Python packages to be used in rgee. This function is
#' a wrapper around `reticulate::py_install()`. Due to recent changes in
#' reticulate it is no longer necessary. ee_install_python_packages and
#' ee_install_earthengine_upgrade will be removed in rgee 0.5.3
#'
#' @author Kevin Ushey,  J.J. Allaire, Daniel Falbel, Jan Tilly, Marlin NA.
#'
#' @param method Installation method. By default, "auto" automatically
#' finds a method that will work in the local environment. Change the
#' default to force a specific installation method. Note that the
#' "virtualenv" method is not available on Windows.
#' @param conda The path to a conda executable. Use "auto" to allow
#' reticulate to automatically find an appropriate conda binary. See
#' **Finding Conda** for more details.
#' @param ee_version earthengine-api version to install. When
#' this argument is NULL (the default), the version with which
#' rgee was built will be installed.
#' @param python_version The requested Python version. Ignored when
#'  attempting to install with a Python virtual environment.
#' @param pip Logical. Use pip for package installation? This
#' is only relevant when Conda environments are used, as
#' otherwise packages will be installed from the
#' Conda repositories.
#' @param confirm Logical. Confirm if restart R when the 'install'
#' argument is TRUE.
#' @param ... Additional arguments passed to \link[=reticulate]{conda_install}
#' or \link[=reticulate]{virtualenv_install}.
#' @importFrom reticulate source_python py_install
#' @details It is neccessary restart R to observe change when
#' installing Python packages. rgee only is compatible with Python
#' version 3.5 >=. On Linux and OS X the "virtualenv" method will
#' be used by default ("conda" will be used if virtualenv isn't available). On
#' Windows, the "conda" method is always used.
#'
#' @seealso `reticulate::py_install()`
#'
#' @section Finding Conda:
#'
#' When `conda = "auto"`, `reticulate` will attempt to automatically find an
#' Anaconda / Miniconda installation and use that. `reticulate` will search the
#' following locations:
#'
#' 1. The location specified by the `reticulate.conda_binary` \R option;
#' 2. The program `PATH`;
#' 3. A set of pre-defined locations where Conda is typically installed.
#'
#' @examples
#'
#' \dontrun{
#' #' library(rgee)
#'
#' ### rgee installation
#'
#' # 1. Initialize rgee with ee_Initialize(). If there is no any Python
#' # environment, miniconda will be installed by default.
#' ee_Initialize()
#'
#' # 2. Create a Python environment, e.g. ee.
#' pyenv <- ee_install_create_pyenv(py_env = "ee")
#'
#' # Find others Python environments in the system.
#' ee_install_discover_pyenvs()
#'
#' # 3. Set a Python environment (e.g. ee) and restart R to see changes.
#' ee_install_set_pyenv(pyenv, install = TRUE)
#'
#' # 4. Install Python package dependencies and restart R to see changes.
#' ee_install_python_packages()
#'
#' # 5. Initialize rgee again!
#' ee_Initialize()
#'
#' }
#' @export
ee_install_python_packages <- function(method = c(
  "auto",
  "virtualenv",
  "conda"
),
conda = "auto",
ee_version = NULL,
python_version = NULL,
pip = FALSE,
confirm = interactive(),
...) {
  .Deprecated(
    msg = paste0(
      "Due to recent changes in reticulate it is no longer necessary.",
      " ee_install_python_packages and  ee_install_earthengine_upgrade",
      " will be removed in rgee 0.5.3"
    ),
  )
  rgee_packages <- c("pyasn1", "oauth2client", "numpy")
  # verify 64-bit
  if (.Machine$sizeof.pointer != 8) {
    stop(
      "Unable to install rgee on this platform.",
      "Binary installation is only available for 64-bit platforms."
    )
  }

  # get the version of the earthengine-api
  if (is.null(ee_version)) {
    ee_version <- ee_version()
  }
  ee_version <- paste0("earthengine-api==", ee_version)

  # verify Python version
  ee_check_python(quiet = TRUE)

  method <- match.arg(method)
  rgee_packages <- unique(rgee_packages)

  py_install(
    packages = c(ee_version, rgee_packages),
    method = method,
    conda = conda,
    python_version = python_version,
    pip = pip,
    ...
  )

  cat("\nInstallation complete.\n\n")

  # restartSession does not work properly
  # if (restart_session && hasFun("restartSession")) {
  #   restartSession()
  # }
  if (isTRUE(confirm)) {
    title <- paste0(
      "rgee needs to restart R session to see changes.\n",
      "Do you want to continues?"
    )
    response <- menu(c("yes", "no"), title = title)
  } else {
    response <- confirm
  }

  switch(response + 1,
         cat("Restart R session to see changes.\n"),
         quit("no"))

  invisible(TRUE)
}

#' Upgrade the Earth Engine Python API (DEPRECATED)
#'
#' Upgrade the Earth Engine Python API (earthengine-api) to the latest
#' version. This function is a wrapper around  `reticulate::py_install()`.
#' Due to recent changes in reticulate it is no longer necessary.
#' ee_install_python_packages and  ee_install_earthengine_upgrade will be
#' removed in rgee 0.5.3
#'
#' @param method Installation method. By default, "auto" automatically
#' finds a method that will work in the local environment. Change the
#' default to force a specific installation method. Note that the
#' "virtualenv" method is not available on Windows.
#' @param conda The path to a conda executable. Use "auto" to allow
#' reticulate to automatically find an appropriate conda binary. See
#' **Finding Conda** for more details.
#' @param python_version The requested Python version. Ignored when
#'  attempting to install with a Python virtual environment.
#' @param pip Logical. Use pip for package installation? This
#' is only relevant when Conda environments are used, as
#' otherwise packages will be installed from the
#' Conda repositories.
#' @param confirm Logical. Confirm if restart R when the 'install'
#' argument is TRUE.
#' @param ... Additional arguments passed to \link[=reticulate]{conda_install}
#' or \link[=reticulate]{virtualenv_install}.
#' @importFrom reticulate source_python py_install
#' @details It is neccessary restart R to observe change when
#' installing Python packages. rgee only is compatible with Python
#' version 3.5 >=. On Linux and OS X the "virtualenv" method will
#' be used by default ("conda" will be used if virtualenv isn't available). On
#' Windows, the "conda" method is always used.
#'
#' @seealso `reticulate::py_install()`
#'
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_install_earthengine_upgrade()
#' }
#' @export
ee_install_earthengine_upgrade <- function(method = c(
  "auto",
  "virtualenv",
  "conda"
),
conda = "auto",
pip = FALSE,
python_version = NULL,
confirm = interactive(),
...) {
  .Deprecated(
    msg = paste0(
      "Due to recent changes in reticulate it is no longer necessary.",
      " ee_install_python_packages and  ee_install_earthengine_upgrade",
      " will be removed in rgee 0.5.3"
    ),
  )
  ee_version <- "earthengine-api"
  py_install(
    packages = ee_version,
    method = method,
    conda = conda,
    python_version = python_version,
    pip = pip,
    ...
  )
  # restartSession does not work properly
  # if (restart_session && hasFun("restartSession")) {
  #   restartSession()
  # }
  if (isTRUE(confirm)) {
    title <- paste0(
      "rgee needs to restart R session to see changes.\n",
      "Do you want to continues?"
    )
    response <- menu(c("yes", "no"), title = title)
  } else {
    response <- confirm
  }

  switch(response + 1,
         cat("Restart R session to see changes.\n"),
         quit("no"))

  invisible(TRUE)
}


#' Create a list of virtuals environments locacted in "~/.virtualenvs"
#' @importFrom reticulate virtualenv_root virtualenv_list
#' @noRd
ee_virtualenv_list <- function() {
  path.expand(
    sprintf("%s/%s/bin/python",
            virtualenv_root(),
            virtualenv_list()))
}
