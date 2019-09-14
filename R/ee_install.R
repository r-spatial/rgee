#' Interface to install non-R rgee dependencies
#'
#' R functions for installing and removing Python Selenium Chromedriver and Third-Party Python packages.
#' @name ee_install-tools
#' @param quiet logical; suppress info message
#' @param user TODO
#' @param version TODO
#' @importFrom reticulate py_available py_module_available py_discover_config source_python
#' @details It is neccessary restart RStudio to observe change when installing a python packages. rgee only is
#' compatible with Python 3.5 >.
#' @examples
#' \dontrun{
#'library(rgee)
#'
#' # Recommended way to install external dependencies of rgee for Linux users
#'virtualenv_remove("rgee")
#'virtualenv_create("rgee", python = "python3.5")
#'use_virtualenv("rgee")
#'
#'ee_check()
#'ee_install_rgee_python_packages() # Install rgee python packages
#'ee_install_drivers() # Install selenim drivers
#'ee_get_credentials() # Get credentials
#'ee_check()
#'
#' # Recommended way to install external dependencies of rgee for Windows users
#' }
#' @export
ee_install_drivers <- function(version) {
  if (missing(version)) {
    stop('version was not defined.',
         " Find the appropriate version of chromedriver visiting:\n",
         ">>> chrome://settings/help",
         ">>> https://sites.google.com/a/chromium.org/chromedriver/downloads\n"
    )
  }
  oauth_func_path <- system.file("python/ee_check_utils.py", package = "rgee")
  ee_check_utils <- ee_source_python(oauth_func_path)
  directory = ee_get_earthengine_path()

  os_type <- switch(Sys.info()[['sysname']],
                    Windows= {'windows'},
                    Linux  = {'linux'},
                    Darwin = {'macos'})
  ee_check_utils$download_chromedriver(directory, os_type, version)
  return(TRUE)
}

#' @rdname ee_install-tools
#' @param conda TODO
#' @export
ee_install_rgee_python_packages <- function(conda = FALSE) {
  ee_install_python_ee(conda = conda)
  ee_install_python_package("selenium", conda = conda)
  ee_install_python_package("bs4", conda = conda)
  ee_install_python_package("pysmartDL", conda = conda)
  ee_install_python_package("requests_toolbelt", conda = conda)
  cat("restart R to see changes")
}

#' @rdname ee_install-tools
#' @param pypackage TODO
#' @export
ee_install_python_package <- function(pypackage,conda=FALSE) {
  pydiscv <- py_discover_config()
  python_version <- pydiscv$version
  if (conda) {
    msg_return <- suppressWarnings(system2("conda"))
    if (msg_return!=0) {
      stop("conda is not installed in the system, try using pip ee_install_python_ee(conda=FALSE)")
    } else {
      system("conda install pypackage --upgrade")
    }
  } else {
    msg_return <- system2(sprintf("python%s",python_version)," -m pip --version")
    if (msg_return!=0) {
      stop("pip is not installed in your system, try using conda ee_install_python_ee(conda=TRUE)")
    } else {
      install <- suppressWarnings(system(sprintf("pip%s install %s --upgrade",python_version,pypackage)))
      if (install !=0) {
        python_version <- substring(python_version,1,1)
        install <- suppressWarnings(system(sprintf("pip%s install %s --upgrade",python_version,pypackage)))
        if (install !=0) {
          stop("An unexpected error occurred when try to use pip")
        }
      }
    }
  }
}

#' @rdname ee_install-tools
#' @export
ee_install_python_ee <- function(conda=FALSE) {
  pydiscv <- py_discover_config()
  python_version <- pydiscv$version
  if (conda) {
    msg_return <- suppressWarnings(system2("conda"))
    if (msg_return!=0) {
      stop("conda is not installed in the system, try using pip ee_install_python_ee(conda=FALSE, restart=TRUE)")
    } else {
      system("conda install earthengine-api==0.1.175 --upgrade")
    }
  } else{
    msg_return <- system2(sprintf("python%s",python_version)," -m pip --version")
    if (msg_return!=0) {
      stop("pip is not installed in your system, try using conda ee_install_python_ee(conda=TRUE)")
    } else {
      install <- suppressWarnings(system2(sprintf("pip%s",python_version),"install earthengine-api==0.1.175 --upgrade"))
      if (install !=0) {
        python_version <- substring(python_version,1,1)
        install <- system2(sprintf("pip%s",python_version),"install earthengine-api==0.1.175 --upgrade")
        if (install !=0) {
          stop("An unexpected error occurred when try to use pip")
        }
      }
    }
  }
}


#' @rdname ee_install-tools
#' @export
ee_remove_drivers <- function(quiet = TRUE) {
  path <- ee_get_earthengine_path()
  gecko_driver_linux <- sprintf("%s/geckodriver",path)
  gecko_driver_win <- sprintf("%s/googledrive.exe",path)
  if (file.exists(gecko_driver_win)) file.remove(gecko_driver_win)
  if (file.exists(gecko_driver_linux)) file.remove(gecko_driver_linux)
  if (!quiet) cat(sprintf("Credentials in %s has been removed.",path))
}

#' @rdname ee_install-tools
#' @export
ee_remove_credentials <- function(user , quiet = TRUE) {
  ee_path <- path.expand("~/.config/earthengine/")
  unlink(paste0(ee_path,user),recursive = TRUE)
  invisible(TRUE)
}
