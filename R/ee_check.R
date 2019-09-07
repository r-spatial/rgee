#' Interface to handle rgee installation requirements
#'
#' R functions for checking, installing and removing: drivers (geckodrivers), credentials and python packages.
#' @param quiet logical; suppress info message
#' @param user TODO
#' @importFrom reticulate py_available py_module_available py_discover_config source_python
#' @name ee_check-tools
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
ee_check <- function() {
  ee_check_python()
  ee_check_rgee_python_packages()
  ee_check_drivers(display_in_browser = F)
  ee_check_credentials()
}

#' @rdname ee_check-tools
#' @export
ee_check_python <- function() {
  python_test <- py_available(initialize = TRUE)
  if (python_test) {
    cat(">>> The python version is:", py_discover_config()$python, "\n")
  } else {
    stop("Unable to find a python version, you will need to fix it before installing rgee. ",
         "More details through reticulate::py_available().")
  }
  py_version <- as.numeric(substring(py_discover_config()$version,1,1))
  if (py_version!=3) stop("rgee just run under python3")
  return(TRUE)
}


#' @rdname ee_check-tools
#' @export
ee_check_rgee_python_packages <- function() {
  oauth_func_path <- system.file("python/ee_check_utils_exist.py", package = "rgee")
  ee_check_utils_exist <- ee_source_python(oauth_func_path)

  cat("\n")
  cat(">>> python Standard Libraries used in rgee: \n")
  # webbrowser
  ee_check_rgee_package("webbrowser")
  ee_check_rgee_package("request")
  ee_check_rgee_package("zipfile")
  ee_check_rgee_package("tarfile")
  ee_check_rgee_package("subprocess")
  ee_check_rgee_package("retrying")
  ee_check_rgee_package("ast")
  ee_check_rgee_package("sys")
  ee_check_rgee_package("os")
  ee_check_rgee_package("platform")
  ee_check_rgee_package("json")

  cat("\n")
  cat(">>> python Third-Party Libraries used in rgee: \n")

  #ee
  version_ee <- ee_check_utils_exist$ee_check_py_ee()
  ee_cond <- is.character(version_ee)
  if (ee_cond) {
    if (version_ee == "0.1.175") {
      cat(sprintf("    - ee -> status[ok]: v%s\n",version_ee))
    } else {
      ee_message <- sprintf("Earth Engine python API (ee) %s is installed correctly in the system,%s. %s",
                            version_ee,
                            "but rgee depends on 0.1.175. Please run ee_install_python_ee() for upgrading",
                            "If the installation is successful, restart to see changes.")
      stop(ee_message)
    }
  } else {
    cat("    - ee -> status[X]\n")
  }

  ee_check_rgee_package("selenium")
  ee_check_rgee_package("bs4")
  ee_check_rgee_package("pysmartDL")
  ee_check_rgee_package("requests_toolbelt")
}

#' @rdname ee_check-tools
#' @param display_in_browser TODO
#' @export
ee_check_drivers <- function(display_in_browser = TRUE) {
  oauth_func_path <- system.file("python/ee_check_utils.py", package = "rgee")
  ee_check_utils <- ee_source_python(oauth_func_path)
  driverdir <- ee_get_earthengine_path()
  condition <- ee_py_to_r(ee_check_utils$ee_check_drivers_py(driverdir, display_in_browser))
  if (condition) {
    cat("\n")
    cat(">>> Chromedriver -> status[ok]:", driverdir,"\n")
  } else {
    warning(">>> Chromedriver -> status[X]. Try rgee::ee_install_drivers() to fixed.","\n")
  }
}

#' @rdname ee_check-tools
#' @export
ee_check_credentials <- function() {
  driverdir <- ee_get_earthengine_path()
  ee_credentials <- sprintf("%s/credentials",driverdir)
  drive_credentials <- sprintf("%s/googledrive",driverdir)
  ex_ee_cred <- file.exists(ee_credentials)
  ex_drive_cred <- file.exists(drive_credentials)
  if (sum(ex_drive_cred,ex_ee_cred) == 2) {
    cat("\n")
    cat(">>> Credentials -> status[ok]:",driverdir,"\n")
  } else {
    warning(">>> Credentials -> status[X]. Try rgee::ee_get_credentials() to fixed.","\n")
  }
}


#' @rdname ee_check-tools
#' @param version TODO
#' @export
ee_install_drivers <- function(version) {
  if (is.null(version)) {
    stop(" Find the appropriate version of chromedriver visiting:\n",
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

#' @rdname ee_check-tools
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

#' @rdname ee_check-tools
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

#' @rdname ee_check-tools
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

#' @rdname ee_check-tools
#' @export
ee_get_earthengine_path <- function() {
  ee_path <- path.expand("~/.config/earthengine")
  sessioninfo <- sprintf("%s/rgee_sessioninfo.txt", path.expand("~/.config/earthengine"))
  if (file.exists(sessioninfo)) {
    user <- read.table(sessioninfo,header = TRUE,stringsAsFactors = FALSE)[[1]]
  } else {
    user <- gsub("users/","",ee$data$getAssetRoots()[[1]]$id)
  }
  return(sprintf("%s/%s/",ee_path,user))

}

#' Check python packages
#' @param rgee_package package name to install
#' @export
ee_check_rgee_package <- function(rgee_package) {
  oauth_func_path <- system.file("python/ee_check_utils_exist.py", package = "rgee")
  ee_source_python(oauth_func_path)
  version_rgeepackage <- eval(parse(text = sprintf("ee_check_py_%s()",rgee_package)))
  rgeepackage_is_text <- is.character(version_rgeepackage)
  rgeepackage_is_TRUE <- isTRUE(version_rgeepackage)
  if (rgeepackage_is_text) {
    cat(sprintf("    - %s -> status[ok]: v%s\n",rgee_package, version_rgeepackage))
  }
  if (rgeepackage_is_TRUE) {
    cat(sprintf("    - %s -> status[ok]\n",rgee_package))
  }
  if (isFALSE(version_rgeepackage)) {
    cat(sprintf("    - %s -> status[X]\n",rgee_package))
    stop(sprintf("%s has not been installed in this python version, try as follow for fixed: \n",rgee_package),
         sprintf(">>> rgee::ee_install_python_package('%s', conda = FALSE),",rgee_package),
          " If the installation is successful, restart to see changes.")
  }
}

#' @rdname ee_check-tools
#' @export
ee_remove_drivers <- function(quiet = TRUE) {
  path <- ee_get_earthengine_path()
  gecko_driver_linux <- sprintf("%s/geckodriver",path)
  gecko_driver_win <- sprintf("%s/googledrive.exe",path)
  if (file.exists(gecko_driver_win)) file.remove(gecko_driver_win)
  if (file.exists(gecko_driver_linux)) file.remove(gecko_driver_linux)
  if (!quiet) cat(sprintf("Credentials in %s has been removed.",path))
}


#' @rdname ee_check-tools
#' @export
ee_remove_credentials <- function(user , quiet = TRUE) {
  ee_path <- path.expand("~/.config/earthengine/")
  unlink(paste0(ee_path,user),recursive = TRUE)
  invisible(TRUE)
}
