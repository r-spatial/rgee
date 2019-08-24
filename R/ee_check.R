#' Interface to handle Installation requirements
#'
#' R functions for checking, installing and removing: drivers (geckodrivers), credentials and python packages.
#'
#'
#' @importFrom reticulate py_available py_module_available py_discover_config source_python
#' @importFrom googledrive drive_auth
#' @name ee_check-tools
#' @details It is neccessary restart RStudio to observe change when installing a python packages. Rgee only is
#' compatible with python3.
#' @examples
#' \dontrun{
#'library(rgee)
#'
#'#The recommended way to use of rgee
#'virtualenv_remove("rgee")
#'virtualenv_create("rgee", python = "python3.5")
#'use_virtualenv("rgee")
#'
#'ee_check()
#'ee_install_rgee_python_packages() # Install rgee python packages
#'ee_install_drivers() # Install selenim drivers
#'ee_get_credentials() # Get credentials
#'ee_check()
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
      ee_message <- sprintf("Earth Engine python API (ee) %s is installed correctly in your system,%s. %s",
                            version_ee,
                            "but rgee depends on 0.1.175. Please run ee_install_ee() for upgrading",
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
  condition <- ee_check_utils$ee_check_drivers_py(driverdir, display_in_browser)
  if (condition) {
    cat("\n")
    cat(">>> Geckodriver -> status[ok]:",driverdir,"\n")
  } else {
    warning(">>> Geckodriver -> status[X]. Try rgee::ee_install_drivers() to fixed.","\n")
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
ee_install_drivers <- function(version='latest') {
  oauth_func_path <- system.file("python/ee_check_utils.py", package = "rgee")
  ee_check_utils <- ee_source_python(oauth_func_path)
  if (version == 'latest') version = NULL
  directory = dirname(gd_cre_path())
  os_type = Sys.info()[['sysname']]
  if (os_type == "Linux") {
    ee_check_utils$geckodown_linux(directory,version)
  } else if(os_type == "Windows") {
    ee_check_utils$geckodown_win(directory,version)
  } else {
    ee_check_utils$geckodown_mac(directory,version)
  }
}

#' @rdname ee_check-tools
#' @param conda TODO
#' @export
ee_install_rgee_python_packages <- function(conda = FALSE) {
  ee_install_ee(conda = conda)
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
      stop("conda is not installed in the system, try using pip ee_install_ee(conda=FALSE)")
    } else {
      system("conda install pypackage --upgrade")
    }
  } else {
    msg_return <- system2(sprintf("python%s",python_version)," -m pip --version")
    if (msg_return!=0) {
      stop("pip is not installed in your system, try using conda ee_install_ee(conda=TRUE)")
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
ee_install_ee <- function(conda=FALSE) {
  pydiscv <- py_discover_config()
  python_version <- pydiscv$version
  if (conda) {
    msg_return <- suppressWarnings(system2("conda"))
    if (msg_return!=0) {
      stop("conda is not installed in the system, try using pip ee_install_ee(conda=FALSE, restart=TRUE)")
    } else {
      system("conda install earthengine-api==0.1.175 --upgrade")
    }
  } else{
    msg_return <- system2(sprintf("python%s",python_version)," -m pip --version")
    if (msg_return!=0) {
      stop("pip is not installed in your system, try using conda ee_install_ee(conda=TRUE)")
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
  return(path.expand("~/.config/earthengine"))
}




#' @rdname ee_check-tools
#' @export
ee_get_credentials <- function() {
  oauth_func_path <- system.file("python/ee_get_credentials.py", package = "rgee")
  ee_get_credentials <- ee_source_python(oauth_func_path)
  credential_path <-  ee_get_earthengine_path()
  gd_credentials <- sprintf("%s/googledrive", credential_path)
  gee_credentials <- sprintf("%s/credentials", credential_path)

  ee_get_credentials$ee_authenticate_py()
  code <- readline("Enter authorisation code for Earth Engine API here: ")
  test <- try(ee_get_credentials$request_ee_token_py(code), silent = T)
  saveRDS(
    drive_auth(cache = F),
    gd_cre_path()
  )
}


#' @rdname ee_check-tools
#' @param quiet TODO
#' @export
ee_remove_credentials <- function(quiet = TRUE) {
  path <- ee_get_earthengine_path()
  ee_credential <- sprintf("%s/credentials",path)
  drive_credential <- sprintf("%s/googledrive",path)
  if (file.exists(drive_credential)) file.remove(drive_credential)
  if (file.exists(ee_credential)) file.remove(ee_credential)
  if (!quiet) cat(sprintf("Credentials in %s has been removed.",path))
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


