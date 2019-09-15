#' Interface to install non-R rgee dependencies
#'
#' R functions for installing and removing Python Selenium Chromedriver and Third-Party Python packages.
#' @param GoogleChromeVersion Google Chrome version of this system.
#' @param user character. User to remove credentials
#' @param conda If TRUE conda is used instead of pip. conda executable needs setting in PATH.
#' @param pypackage character. Package names to install.
#' @param quiet logical. Suppress info message
#' @importFrom reticulate py_available py_module_available py_discover_config source_python
#' @details It is neccessary restart R to observe change when installing a Python packages. rgee only is
#' compatible with Python version 3.5 >.
#' @name ee_install-tools
#' @examples
#' \dontrun{
#' library(rgee)
#'
#' # Recommended way to use rgee for Linux users
#' virtualenv_remove("rgee")
#' virtualenv_create("rgee", python = "python3.7")
#' use_virtualenv("rgee") # Restart R
#'
#' ee_check()
#' ee_install_rgee_python_packages() # Install rgee python packages
#' ee_install_drivers() # Install selenim drivers
#' ee_Initialize(drive=TRUE,gcs=TRUE) # Install credentials
#' ee_check()
#'
#' # Recommended way to install external dependencies of rgee for Windows users
#' }
#' @export
ee_install_drivers <- function(GoogleChromeVersion) {
  if (missing(GoogleChromeVersion)) {
    stop('The GoogleChromeVersion argument was not defined.',
         " Find the appropriate version of Google Chrome visiting:\n",
         ">>> chrome://settings/help \n"
    )
  }
  oauth_func_path <- system.file("python/ee_check_utils.py", package = "rgee")
  ee_check_utils <- ee_source_python(oauth_func_path)
  directory = path.expand("~/.config/earthengine/")

  os_type <- switch(Sys.info()[['sysname']],
                    Windows= {'windows'},
                    Linux  = {'linux'},
                    Darwin = {'macos'})
  chromedriver_version <- ee_check_utils$download_chromedriver(directory = directory,
                                                               operating_system = os_type,
                                                               version = as.character(GoogleChromeVersion))
  cat('Selenium ChromeDriver v',ee_py_to_r(chromedriver_version),"saved in", directory)
  return(invisible(TRUE))
}

#' @rdname ee_install-tools
#' @export
ee_install_rgee_python_packages <- function(conda = FALSE, quiet = quiet) {
  ee_install_python_ee(conda = conda, quiet = quiet)
  ee_install_python_package("selenium", conda = conda, quiet = quiet)
  ee_install_python_package("bs4", conda = conda, quiet = quiet)
  ee_install_python_package("pysmartDL", conda = conda, quiet = quiet)
  ee_install_python_package("requests_toolbelt", conda = conda, quiet = quiet)
  if (!quiet) message("restart R to see changes")
}

#' @rdname ee_install-tools
#' @export
ee_install_python_package <- function(pypackage, conda = FALSE, quiet = FALSE) {
  pydiscv <- py_discover_config()
  python_version <- pydiscv$version
  if (conda) {
    msg_return <- suppressWarnings(system("conda --v", show.output.on.console = FALSE))
    if (msg_return != 1) {
      stop("conda is not installed in the system, try using pip ee_install_python_ee(conda=FALSE)")
    } else {
      to_run = sprintf("conda install -c conda-forge %s --debug", pypackage)
      if (!quiet) cat('Running:',to_run, ' ... please wait\n')
      tryCatch(expr = system(to_run,invisible = FALSE, timeout = 20),
               warning = function(w)
                 stop('Conda exceeded the default timeout. Run in Terminal: ',
                         to_run,' . After that restart R to see changes.'))

    }
  } else {
    msg_return <- system2(sprintf("python%s", python_version), " -m pip --version")
    if (msg_return != 0) {
      stop("pip is not installed, try using conda ee_install_python_ee(conda=TRUE)")
    } else {
      install <- suppressWarnings(
        suppressMessages(
          system(sprintf("pip%s install %s --upgrade", python_version, pypackage), ignore.stderr = TRUE)
        )
      )
      if (install != 0) {
        python_version <- substring(python_version, 1, 1)
        install <- suppressWarnings(
          system(sprintf("pip%s install %s --upgrade", python_version, pypackage))
        )
        if (install != 0) {
          stop("An unexpected error occurred when try to use pip")
        }
      }
    }
  }
  if (!quiet) message("restart R to see changes")
}

#' @rdname ee_install-tools
#' @export
ee_install_python_ee <- function(conda=FALSE, quiet = FALSE) {
  if (conda) {
    msg_return <- suppressWarnings(system("conda", show.output.on.console = FALSE))
    if (msg_return!=1) {
      stop("conda is not installed in the system, try using pip ee_install_python_ee(conda=FALSE, restart=TRUE)")
    } else {
      to_run = "conda install -c conda-forge earthengine-api=0.1.175 --debug"
      if (!quiet) cat('Running:',to_run, ' ... please wait\n')
      tryCatch(expr = system(to_run,invisible = FALSE, timeout = 20),
               warning = function(w)
                 stop('Conda exceeded the default timeout. Run in Terminal: ',
                         to_run,' . After that restart R to see changes.'))
    }
  } else{
    pydiscv <- py_discover_config()
    python_version <- pydiscv$version
    msg_return <- system2(sprintf("python%s",python_version)," -m pip --version")
    if (msg_return!=0) {
      stop("pip is not installed, try using conda ee_install_python_ee(conda=TRUE)")
    } else {
      install <- suppressWarnings(
        suppressMessages(
          system(sprintf("pip%s%s",python_version,"install earthengine-api==0.1.175 --upgrade"), ignore.stderr = TRUE)
        )
      )
      if (install !=0) {
        python_version <- substring(python_version,1,1)
        install <- system2(sprintf("pip%s",python_version),"install earthengine-api==0.1.175 --upgrade")
        if (install !=0) {
          stop("An unexpected error occurred when try to use pip")
        }
      }
    }
  }
  if (!quiet) message("restart R to see changes")
}

#' @rdname ee_install-tools
#' @export
ee_remove_drivers <- function(quiet = TRUE) {
  ee_path <- path.expand("~/.config/earthengine/")
  gecko_driver_linux <- sprintf("%s/chromedriver", ee_path)
  gecko_driver_win <- sprintf("%s/chromedriver.exe", ee_path)
  if (file.exists(gecko_driver_win)) file.remove(gecko_driver_win)
  if (file.exists(gecko_driver_linux)) file.remove(gecko_driver_linux)
  if (!quiet) cat(sprintf("Credentials in %s has been removed.",ee_path))
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
