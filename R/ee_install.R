#' Create an isolated Python virtual environment to be used in rgee
#' @param python_env The name of, or path to, a Python virtual environment.
#' @importFrom reticulate conda_create virtualenv_create
#' @return Character. The dirpath of the virtual environment created.
#' @examples
#' \dontrun{
#' #' library(rgee)
#'
#' ### rgee installation
#'
#' # 1. Initialize rgee with ee_Initialize(). If there is no any Python environment, miniconda
#' # will be installed by default.
#' ee_Initialize()
#'
#' # 2. Create a Python environment, e.g. ee.
#' pyenv <- ee_install_create_pyenv(python_env = "ee")
#'
#' # Find others Python environments in the system.
#' # ee_install_discover_pyenvs()
#'
#' # 3. Set a Python environment (e.g. ee) and restart R to see changes.
#' ee_install_set_pyenv(pyenv, install = TRUE)
#'
#' # 4. Install Python package dependencies and restart R to see changes.
#' ee_install_python_packages()
#'
#' # 5. Initialize rgee again!
#' ee_Initialize()
#' }
#' @export
ee_install_create_pyenv <- function(python_env = "rgee") {

  #Check is Python is greather than 3.5
  ee_check_python(quiet = TRUE)

  if (is_windows()) {
    pyenv_path <- conda_create(python_env)
  } else {
    pyenv_path <- virtualenv_create(python_env)
  }
  pyenv_path
}

#' Discover all the Python environments available
#' in the system
#'
#' This function enables callers to check which versions
#' of Python will be discovered on a system.
#'
#' @param use_py_discover_config Logical. If TRUE
#' will use \code{\link{reticulate}} to find
#' versions of Python in the system.  Otherwise, will use
#' \link[=reticulate]{conda_list} for Window OS and
#' \link[=reticulate]{virtualenv_list} for Unix system.
#' @importFrom reticulate py_discover_config conda_list
#' @return Python configuration object (reticulate).
#' @examples
#' \dontrun{
#' #' library(rgee)
#'
#' ### rgee installation
#'
#' # 1. Initialize rgee with ee_Initialize(). If there is no any Python environment, miniconda
#' # will be installed by default.
#' ee_Initialize()
#'
#' # 2. Create a Python environment, e.g. ee.
#' pyenv <- ee_install_create_pyenv(python_env = "ee")
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
#' }
#' @export
ee_install_discover_pyenvs <- function(use_py_discover_config = FALSE) {
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
#' @param python_env The name of, or path to, a Python virtual environment. If
#' not defined will estimate from the path.
#' @param automatic_pyenv Logical. Search automatically in the python_path the
#' python_env. Ignore when the \code{python_env} argument is not NULL. By
#' default TRUE.
#' @param install if TRUE, rgee will save the Python interpreter path and
#' the virtual environment name in the \code{.Renviron} file
#' for use in future sessions. Defaults to FALSE.
#' @param confirm Logical. Confirm if restart R when the 'install'
#' argument is TRUE.
#'
#' @importFrom utils menu
#' @details It is necessary to restart R to observe change when setting a
#' different Python version. ee_install_set_pyenv will ask you to restart R.
#' @examples
#' \dontrun{
#' #' library(rgee)
#'
#' ### rgee installation
#'
#' # 1. Initialize rgee with ee_Initialize(). If there is no any Python environment, miniconda
#' # will be installed by default.
#' ee_Initialize()
#'
#' # 2. Create a Python environment, e.g. ee.
#' pyenv <- ee_install_create_pyenv(python_env = "ee")
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
#' }
#' @export
ee_install_set_pyenv <- function(python_path,
                         python_env = NULL,
                         automatic_pyenv = TRUE,
                         install = FALSE,
                         confirm = interactive()) {
  ee_clean_pyenv()
  # Trying to get the env from the python_path
  if (is.null(python_env) & automatic_pyenv) {
    if (grepl("\\.virtualenvs/", python_path)) {
      python_env <- gsub(".*\\.virtualenvs\\/(.*)", "\\1", python_path) %>%
        strsplit("/") %>%
        "[["(1) %>%
        "["(1)
    } else if (grepl("\\.conda.envs", python_path)) {
      python_path <- normalizePath(python_path, "/")
      python_env <- gsub(".*\\.conda\\/envs\\/(.*)", "\\1", python_path) %>%
        strsplit("/") %>%
        "[["(1) %>%
        "["(1)
    } else if (grepl("r-miniconda.envs", python_path)) {
      python_path <- normalizePath(python_path, "/")
      python_env <- gsub(".*\\/r-miniconda\\/envs\\/(.*)", "\\1", python_path) %>%
        strsplit("/") %>%
        "[["(1) %>%
        "["(1)
    }
    if (is.null(python_env)) {
      message("python_env is NULL, RETICULATE_PYTHON_ENV will not be created.")
    } else {
      message(
        "Establishing the python virtual environment (python_env) as ",
        python_env, "."
      )
    }
  }


  if (isTRUE(install)) {
    home <- Sys.getenv("HOME")
    renv <- file.path(home, ".Renviron")

    if (file.exists(renv)) {
      # Backup original .Renviron before doing anything else here.
      file.copy(renv, file.path(home, ".Renviron_backup"))
    }

    if (!file.exists(renv)) {
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
#' Install the necessary Python packages to be used in rgee. This function is
#' a wrapper around `reticulate::py_install()`.
#'
#' @author Kevin Ushey,  J.J. Allaire, Daniel Falbel, Jan Tilly, Marlin NȺ.
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
#' # 1. Initialize rgee with ee_Initialize(). If there is no any Python environment, miniconda
#' # will be installed by default.
#' ee_Initialize()
#'
#' # 2. Create a Python environment, e.g. ee.
#' pyenv <- ee_install_create_pyenv(python_env = "ee")
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

#' Upgrade the Earth Engine Python API
#'
#' Upgrade the Earth Engine Python API (earthengine-api) to the latest
#' version. This function is a wrapper around  `reticulate::py_install()`.
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
