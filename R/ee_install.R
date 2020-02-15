#' Create an isolated Python virtual environment to be used
#' in rgee
#' @param python_env The name of, or path to, a Python virtual environment.
#' @importFrom reticulate conda_create virtualenv_create
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_create_pyenv('ee')
#' }
#' @export
ee_create_pyenv <- function(python_env) {
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
  if (os_type == "linux" | os_type == "macos") {
    virtualenv_create(python_env)
  } else {
    conda_create(python_env)
  }
  invisible(TRUE)
}


#' Discover all the Python environments available
#' in the system
#'
#' This function enables callers to check which versions
#' of Python will be discovered on a system. This function
#' was adapted from \code{\link[reticulate]{py_discover_config}}.
#'
#' @importFrom reticulate py_discover_config
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_reattach() # reattach ee as a reserved word
#' ee_discover_pyenvs()
#' }
#' @export
ee_discover_pyenvs <- function() {
  ret_info <- py_discover_config()
  if (!is.null(ret_info$forced)) {
    cat(
      "NOTE: Python version was forced by",
      ret_info$forced,
      "consider remove this environment variable",
      "to display more options.",
      ""
    )
  }
  print(ret_info$python_versions)
}

#' Set the Python environment to be used on rgee
#'
#'
#' @param python_path The path to a Python interpreter, to be used with rgee.
#' @param python_env The name of, or path to, a Python virtual environment.
#' @importFrom utils menu
#' @details It is necessary to restart R to observe change when setting a
#' different Python version. ee_set_python_version will ask you to restart R.
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_reattach() # reattach ee as a reserved word
#' ee_discover_python_versions()
#' ee_set_python_version(
#'   python_path = "user/.virtualenvs/ee/bin/python",
#'   python_env = 'ee'
#' )
#' }
#' @export
ee_set_pyenv <- function(python_path, python_env) {
  reticulate_dir <- path.expand("~/.Renviron")

  # RETICULATE_PYTHON & RETICULATE_PYTHON_ENV
  ret_python <- sprintf('RETICULATE_PYTHON="%s"', python_path)
  ret_python_env <- sprintf('RETICULATE_PYTHON_ENV="%s"',python_env)
  sink(reticulate_dir)
  cat(ret_python)
  cat("\n")
  cat(ret_python_env)
  sink()
  # restartSession does not work properly
  # if (restart_session && hasFun("restartSession")) {
  #   restartSession()
  # }
  title <- paste0(
    "rgee needs to restart R session to see changes.\n",
    "Do you want to continues?"
  )
  response <- menu(c("Yes", "No"), title = title)
  switch(response + 1,
    cat("Restart R session to see changes.\n"),
    quit("no"),
    cat("Restart R session to see changes.\n")
  )
  invisible(TRUE)
}

#' Install Python packages dependecies
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
#' @param conda_python_version the Python version installed in the
#' created conda environment. Python 3.7 is installed by default.
#' @param ... other arguments passed to [reticulate::conda_install()] or
#' [reticulate::virtualenv_install()].
#' @param quiet logical. Suppress info message
#' @importFrom reticulate source_python py_install
#' @details It is neccessary restart R to observe change when
#' installing Python packages. rgee only is compatible with Python
#' version 3.5 >=.
#' @export
ee_install_python_packages <- function(method = c(
                                         "auto",
                                         "virtualenv",
                                         "conda"
                                       ),
                                       conda = "auto",
                                       ee_version = NULL,
                                       envname = NULL,
                                       conda_python_version = "3.7",
                                       quiet = FALSE,
                                       ...) {
  rgee_packages <- c(
    "selenium", "bs4", "pysmartDL",
    "requests_toolbelt", "oauth2client"
  )
  # verify 64-bit
  if (.Machine$sizeof.pointer != 8) {
    stop(
      "Unable to install rgee on this platform.",
      "Binary installation is only available for 64-bit platforms."
    )
  }

  if (is.null(ee_version)) {
    ee_version <- ee_version()
  }

  # verify Python version
  ee_check_python(quiet = quiet)

  method <- match.arg(method)
  rgee_packages <- unique(rgee_packages)
  if (ee_version == "latest") {
    ee_version <- "earthengine-api"
  } else {
    ee_version <- paste0("earthengine-api==", ee_version)
  }

  py_install(
    packages = c(ee_version, rgee_packages),
    envname = envname,
    method = method,
    conda = conda,
    python_version = conda_python_version,
    pip = TRUE,
    ...
  )

  cat("\nInstallation complete.\n\n")

  # restartSession does not work properly
  # if (restart_session && hasFun("restartSession")) {
  #   restartSession()
  # }
  title <- paste0(
    "rgee needs to stop R session to see changes.\n",
    "Do you want to continues?"
  )
  response <- menu(c("Yes", "No"), title = title)
  switch(response + 1,
    cat("Restart R session to see changes.\n"),
    quit("no"),
    cat("Restart R session to see changes.\n")
  )

  invisible(TRUE)
}


#' Install ChromeDriver in the system
#'
#' Install ChromeDriver in the system
#' @param ChromeDriverVersion Google Chrome version of the system.
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_install_ChromeDriver()
#' }
#' @seealso \code{\link[rgee]{ee_upload}}
#' @export
ee_install_ChromeDriver <- function(ChromeDriverVersion) {
  if (missing(ChromeDriverVersion)) {
    stop(
      "The ChromeDriverVersion argument was not defined.",
      " Find the appropriate version of Google Chrome visiting:\n",
      "- chrome://settings/help \n",
      "- After that run,for e.g, rgee::ee_install_drivers(77)"
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
    version = substr(as.character(ChromeDriverVersion), 1, 2)
  )
  cat(
    "Selenium ChromeDriver v",
    ee_py_to_r(chromedriver_version),
    "saved in",
    directory
  )
  return(invisible(TRUE))
}
