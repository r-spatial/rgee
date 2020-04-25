#' Create an isolated Python virtual environment to be used in rgee
#' @param python_env The name of, or path to, a Python virtual environment.
#' @importFrom reticulate conda_create virtualenv_create
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_create_pyenv('ee')
#' ee_discover_pyenvs()
#' ee_set_pyenv(
#'   python_path = ".../ee/bin/python",
#'   python_env = 'ee')
#' ee_install_python_packages()
#' }
#' @export
ee_create_pyenv <- function(python_env) {
  if (is_windows()) {
    conda_create(python_env)
  } else {
    virtualenv_create(python_env)
  }
  invisible(TRUE)
}

#' Discover all the Python environments available
#' in the system
#'
#' This function enables callers to check which versions
#' of Python will be discovered on a system.
#'
#' @param use_py_discover_config Logical. If TRUE
#' will use
#' \code{reticulate::\link[reticulate:py_discover_config]{py_discover_config}}
#' to find versions of Python in the system.  Otherwise, will use
#' \code{reticulate::\link[reticulate:conda_list]{conda_list}}
#' for Window OS and
#' \code{reticulate::\link[reticulate:virtualenv_list]{virtualenv_list}}
#' for Unix system.
#' @importFrom reticulate py_discover_config conda_list
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_create_pyenv('ee')
#' ee_discover_pyenvs()
#' ee_set_pyenv(
#'   python_path = ".../ee/bin/python",
#'   python_env = 'ee')
#' ee_install_python_packages()
#' }
#' @export
ee_discover_pyenvs <- function(use_py_discover_config = FALSE) {
  if (is_windows()) {
    if (isTRUE(use_py_discover_config)) {
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
      return(ret_info$python_versions)
    } else {
      return(conda_list()$python)
    }
  } else {
    if (isTRUE(use_py_discover_config)) {
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
      return(ret_info$python_versions)
    } else {
      ret_info <- ee_virtualenv_list()
      return(ret_info)
    }
  }
}

#' Set the Python environment to be used on rgee
#'
#' @param python_path The path to a Python interpreter, to be used with rgee.
#' @param python_env The name of, or path to, a Python virtual environment.
#' @param install if TRUE, rgee will save the Python interpreter path and
#' the virtual environment name in the \code{.Renviron} file
#' for use in future sessions. Defaults to FALSE.
#' @param confirm Logical. Confirm if restart R when the 'install'
#' argument is TRUE.
#'
#' @importFrom utils menu
#' @details It is necessary to restart R to observe change when setting a
#' different Python version. ee_set_pyenv will ask you to restart R.
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_create_pyenv('ee')
#' ee_discover_pyenvs()
#' ee_set_pyenv(
#'   python_path = ".../ee/bin/python",
#'   python_env = 'ee')
#' ee_install_python_packages()
#' }
#' @export
ee_set_pyenv <- function(python_path,
                         python_env = NULL,
                         install = FALSE,
                         confirm = interactive()) {
  ee_clean_pyenv()
  if (isTRUE(install)) {
    home <- Sys.getenv("HOME")
    renv <- file.path(home, ".Renviron")

    if(file.exists(renv)){
      # Backup original .Renviron before doing anything else here.
      file.copy(renv, file.path(home, ".Renviron_backup"))
    }
    if(!file.exists(renv)){
      file.create(renv)
    }

    con  <- file(renv, open = "r+")
    lines <- as.character()
    ii <- 1

    while (TRUE) {
      line <- readLines(con, n = 1, warn = FALSE)
      if (length(line) == 0) {
        break()
      }
      lines[ii] <- line
      ii <- ii + 1
    }

    # RETICULATE_PYTHON & RETICULATE_PYTHON_ENV
    ret_python <- sprintf('RETICULATE_PYTHON="%s"', python_path)
    if (is.null(python_env)) {
      system_vars <- c(lines, ret_python)
    } else {
      ret_python_env <- sprintf('RETICULATE_PYTHON_ENV="%s"',python_env)
      system_vars <- c(lines, ret_python, ret_python_env)
    }
    writeLines(system_vars, con)
    close(con)

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
  } else {
    message("To install this Python environment for use ",
            "in future sessions, run this function",
            " with `install = TRUE`.")
    Sys.setenv(RETICULATE_PYTHON = python_path)
    if (!is.null(python_env)) {
      Sys.setenv(RETICULATE_PYTHON_ENV = python_env)
    }
  }
  invisible(TRUE)
}

#' Install rgee Python packages dependencies
#'
#' Install the necessary Python packages to be used in rgee.
#'
#' @param method Installation method. By default, "auto" automatically
#' finds a method that will work in the local environment. Change the
#' default to force a specific installation method. Note that the
#' "virtualenv" method is not available on Windows (as this isn't
#' supported by rgee). Note also that since this command runs
#' without privilege the "system" method is available only on Windows.
#' @param conda Path to conda executable (or "auto" to find conda
#' using the PATH and other conventional install locations).
#' @param ee_version earthengine-api version to install. When
#' this argument is NULL (the default), the version with which
#' rgee was built will be installed.
#' @param envname Name of Python environment, or full path, which Python
#' packages are to be installed.
#' @param pip Logical. Use pip for package installation? This
#' is only relevant when Conda environments are used, as
#' otherwise packages will be installed from the
#' Conda repositories.
#' @param conda_python_version the Python version installed in the
#' created conda environment. Python 3.7 is installed by default.
#' @param ... other arguments passed to
#' \code{reticulate::\link[reticulate:conda_install]{conda_install}}
#' or
#' \code{reticulate::\link[reticulate:virtualenv_install]{virtualenv_install}}
#' @param quiet logical. Suppress info message
#' @importFrom reticulate source_python py_install
#' @details It is neccessary restart R to observe change when
#' installing Python packages. rgee only is compatible with Python
#' version 3.5 >=.
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_create_pyenv('ee')
#' ee_discover_pyenvs()
#' ee_set_pyenv(
#'   python_path = ".../ee/bin/python",
#'   python_env = 'ee')
#' ee_install_python_packages()
#' }
#' @export
ee_install_python_packages <- function(method = c(
                                         "auto",
                                         "virtualenv",
                                         "conda"
                                       ),
                                       conda = "auto",
                                       ee_version = NULL,
                                       envname = NULL,
                                       pip = TRUE,
                                       conda_python_version = "3.7",
                                       quiet = FALSE,
                                       ...) {
  rgee_packages <- "oauth2client"
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
  ee_check_python(quiet = quiet)

  method <- match.arg(method)
  rgee_packages <- unique(rgee_packages)

  py_install(
    packages = c(ee_version, rgee_packages),
    envname = envname,
    method = method,
    conda = conda,
    python_version = conda_python_version,
    pip = pip,
    ...
  )

  cat("\nInstallation complete.\n\n")

  # restartSession does not work properly
  # if (restart_session && hasFun("restartSession")) {
  #   restartSession()
  # }
  invisible(TRUE)
}

#' Upgrade the Earth Engine Python API
#'
#' This function upgrade the Earth Engine Python API (earthengine-api)
#' to the latest version.
#'
#' @param method Installation method. By default, "auto" automatically
#' finds a method that will work in the local environment. Change the
#' default to force a specific installation method. Note that the
#' "virtualenv" method is not available on Windows (as this isn't
#' supported by rgee). Note also that since this command runs
#' without privilege the "system" method is available only on Windows.
#' @param conda Path to conda executable (or "auto" to find conda
#' using the PATH and other conventional install locations).
#' @param envname Name of Python environment, or full path, which Python
#' packages are to be installed.
#' @param pip Logical. Use pip for package installation? This
#' is only relevant when Conda environments are used, as
#' otherwise packages will be installed from the
#' Conda repositories.
#' @param conda_python_version the Python version installed in the
#' created conda environment. Python 3.7 is installed by default.
#' @param ... other arguments passed to
#' \code{reticulate::\link[reticulate:conda_install]{conda_install}}
#' or
#' \code{reticulate::\link[reticulate:virtualenv_install]{virtualenv_install}}.
#' @param quiet logical. Suppress info message
#' @importFrom reticulate source_python py_install
#' @details It is neccessary restart R to observe change when
#' installing Python packages. rgee only is compatible with Python
#' version 3.5 >=.
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_earthengine_upgrade()
#' }
#' @export
ee_earthengine_upgrade <- function(method = c(
                                      "auto",
                                      "virtualenv",
                                      "conda"
                                   ),
                                   conda = "auto",
                                   envname = NULL,
                                   pip = TRUE,
                                   conda_python_version = "3.7",
                                   quiet = FALSE,
                                   ...) {
  ee_version <- "earthengine-api"
  py_install(
    packages = ee_version,
    envname = envname,
    method = method,
    conda = conda,
    python_version = conda_python_version,
    pip = pip,
    ...
  )
  invisible(TRUE)
}

#' Detect the Operating System type of the system
#' @noRd
ee_detect_os <- function() {
  os_type <- switch(Sys.info()[["sysname"]],
                    Windows = {"windows"},
                    Linux = {"linux"},
                    Darwin = {"macos"})
  os_type
}

#' Is the OS windows?
#' @noRd
is_windows <- function() {
  ee_detect_os() == 'windows'
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

