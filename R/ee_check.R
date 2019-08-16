#' Interface to handle Installation requirements
#'
#' R functions for handle: drivers (geckodrivers), credentials and python packages.
#'
#' @importFrom reticulate py_available py_module_available
#' @name ee_check-tools
#' @export
ee_check <- function() {
  ee_check_python()
  ee_check_python_modules()
  ee_check_drivers()
  ee_check_credentials()
}

#' @rdname ee_check-tools
#' @export
ee_check_python <- function() {
  python_test <- py_available(initialize = TRUE)
  if (python_test) {
    cat("• The Python version is:", py_discover_config()$python, "\n")
  } else {
    stop("Unable to find a Python version, you will need to fix it before installing rgee. ",
         "More details through reticulate::py_available().")
  }
  return(TRUE)
}

#' @rdname ee_check-tools
#' @export
ee_check_python_modules <- function(){
  oauth_func_path <- system.file("Python/ee_check_utils.py", package = "rgee")
  ee_source_python(oauth_func_path)
  black_list_standard <- NULL
  black_list_thirdpackage <- NULL
  cat("\n")
  cat("• Python Standard Libraries used in rgee: \n")
  # webbrowser
  if (ee_check_py_webbrowser()) {
    cat("    - webbrowser → status[✓]\n")
     } else {
    cat("    - webbrowser → status[☓]\n")
    black_list_standard <- c(black_list_standard,'webbowser')
  }

  # request
  if (ee_check_py_request()) {
    cat("    - request → status[✓]\n")
  } else {
    cat("    - request → status[☓]\n")
    black_list_standard <- c(black_list_standard,'request')
  }

  # zipfile
  if (ee_check_py_zipfile()) {
    cat("    - zipfile → status[✓]\n")
  } else {
    cat("    - zipfile → status[☓]\n")
    black_list_standard <- c(black_list_standard,'zipfile')
  }

  # tarfile
  if (ee_check_py_tarfile()) {
    cat("    - tarfile → status[✓]\n")
  } else {
    cat("    - tarfile → status[☓]\n")
    black_list_standard <- c(black_list_standard,'tarfile')
  }

  # subprocess
  if (ee_check_py_subprocess()) {
    cat("    - subprocess → status[✓]\n")
  } else {
    cat("    - subprocess → status[☓]\n")
    black_list_standard <- c(black_list_standard,'zipfile')
  }

  # retrying
  if (ee_check_py_retrying()) {
    cat("    - retrying → status[✓]\n")
  } else {
    cat("    - retrying → status[☓]\n")
    black_list_standard <- c(black_list_standard,'retrying')
  }

  # ast
  if (ee_check_py_ast()) {
    cat("    - ast → status[✓]\n")
  } else {
    cat("    - ast → status[☓]\n")
    black_list_standard <- c(black_list_standard,'ast')
  }

  # sys
  if (ee_check_py_sys()) {
    cat("    - sys → status[✓]\n")
  } else {
    cat("    - sys → status[☓]\n")
    black_list_standard <- c(black_list_standard,'sys')
  }

  # os
  if (ee_check_py_os()) {
    cat("    - os → status[✓]\n")
  } else {
    cat("    - os → status[☓]\n")
    black_list_standard <- c(black_list_standard,'os')
  }

  # time
  if (ee_check_py_time()) {
    cat("    - time → status[✓]\n")
  } else {
    cat("    - time → status[☓]\n")
    black_list_standard <- c(black_list_standard,'time')
  }
  cat("\n")
  cat("• Python Third-Party Libraries used in rgee: \n")
  #ee
  version_ee <- ee_check_py_ee()
  ee_cond <- is.character(version_ee)
  if (ee_cond) {
    if (version_ee == "0.1.175") {
      cat(sprintf("    - ee → status[✓]: v%s\n",version_ee))
    } else {
      ee_message <- sprintf("Earth Engine Python API (ee) %s is installed correctly in your system,%s",
                            version_ee,
                            "but rgee depends on 0.1.175. Please run ee_install_ee() for upgrading.")
      warning(ee_message)
    }
  } else {
    cat("    - ee → status[☓]\n")
    black_list_thirdpackage <- c(black_list_thirdpackage, 'ee')
  }

  #selenium
  version_selenium <- ee_check_py_selenium()
  selenium_cond <- is.character(version_selenium)
  if (selenium_cond) {
      cat(sprintf("    - selenium → status[✓]: v%s\n",version_selenium))
  } else {
    cat("    - selenium → status[☓]\n")
    black_list_thirdpackage <- c(black_list_thirdpackage, 'selenium')
  }

  #bs4
  version_bs4 <- ee_check_py_bs4()
  bs4_cond <- is.character(version_bs4)
  if (bs4_cond) {
    cat(sprintf("    - bs4 → status[✓]: v%s\n",version_bs4))
  } else {
    cat("    - bs4 → status[☓]\n")
    black_list_thirdpackage <- c(black_list_thirdpackage, 'bs4')
  }

  #platform
  version_platform <- ee_check_py_platform()
  platform_cond <- is.character(version_platform)
  if (platform_cond) {
    cat(sprintf("    - platform → status[✓]: v%s\n",version_platform))
  } else {
    cat("    - platform → status[☓]\n")
    black_list_thirdpackage <- c(black_list_thirdpackage, 'platform')
  }

  #json
  version_json <- ee_check_py_json()
  json_cond <- is.character(version_json)
  if (json_cond) {
    cat(sprintf("    - json → status[✓]: v%s\n",version_json))
  } else {
    cat("    - json → status[☓]\n")
    black_list_thirdpackage <- c(black_list_thirdpackage, 'json')
  }


  #pysmartDL
  version_pysmartDL <- ee_check_py_pysmartDL()
  pysmartDL_cond <- is.character(version_pysmartDL)
  if (pysmartDL_cond) {
    cat(sprintf("    - pysmartDL → status[✓]: v%s\n",version_pysmartDL))
  } else {
    cat("    - pysmartDL → status[☓]\n")
    black_list_thirdpackage <- c(black_list_thirdpackage, 'pysmartDL')
  }

  #requests_toolbelt
  version_requests_toolbelt <- ee_check_py_requests_toolbelt()
  requests_toolbelt_cond <- is.character(version_requests_toolbelt)
  if (requests_toolbelt_cond) {
    cat(sprintf("    - requests_toolbelt → status[✓]: v%s\n",version_requests_toolbelt))
  } else {
    cat("    - requests_toolbelt → status[☓]\n")
    black_list_thirdpackage <- c(black_list_thirdpackage, 'requests_toolbelt')
  }

  # Aggregating missing packages
  rgee_py_packages <- c(black_list_standard, black_list_thirdpackage)
  if (!is.null(rgee_py_packages)) {
    warning("Some packages do not still installed in this Python version, try as follow for fixed: \n",
            paste0(collapse = "\n",sprintf("- ee_install_%s()",black_list_thirdpackage)))
  }
  return(TRUE)
}

#' @rdname ee_check-tools
#' @export
ee_check_drivers <- function() {
  oauth_func_path <- system.file("Python/ee_check_utils.py", package = "rgee")
  ee_source_python(oauth_func_path)
  driverdir <- ee_get_earthengine_path()
  condition <- ee_check_drivers(driverdir)
  if (condition) {
    cat("\n")
    cat("• Geckodriver → status[✓]:",driverdir,"\n")
  } else {
    warning("• Geckodriver → status[☓]. Try rgee::ee_install_drivers() to fixed.","\n")
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
    cat("• Credentials → status[✓]:",driverdir,"\n")
  } else {
    warning("• Credentials → status[☓]. Try rgee::ee_get_credentials() to fixed.","\n")
  }
}


#' @rdname ee_check-tools
#' @export
ee_install_drivers <- function(version='latest') {
  oauth_func_path <- system.file("Python/ee_check_utils.py", package = "rgee")
  ee_source_python(oauth_func_path)
  if (version == 'latest') version = NULL
  directory = dirname(gd_cre_path())
  os_type = Sys.info()[['sysname']]
  if (os_type == "Linux") {
    geckodown_linux(directory,version)
  } else if(os_type == "Windows") {
    geckodown_win(directory,version)
  } else {
    warning("Experimental: Only windows and Linux distribution based on Debian are soported")
    geckodown_linux(directory,version)
  }
}


#' Install rgee dependecies
#'
#' Wrapping to reticulate::py_install(packages = "ee")
#'
#' @author \href{https://github.com/kevinushey}{Kevin Ushey} and \href{https://github.com/jjallaire}{J.J. Allaire}
#'
#' @param python_version The requested Python version.
#' @param conda whether TRUE will use conda instead pip.
#' @examples
#' \dontrun{
#' # Simple install
#' library(rgee)
#' ee_install()
#'
#' # Install Earth Engine Python API in a virtualenv
#' library(reticulate)
#' library(rgee)
#' py_discover_config()
#' virtualenv_create("rgee", python = "python2.7")
#' use_virtualenv("rgee")
#' py_discover_config()
#' ee_install()
#' }
#' @export
ee_install_ee <- function(python_version, conda=FALSE) {
  if (missing(python_version)) {
    pydiscv <- py_discover_config()
    python_version <- pydiscv$version
  }
  if (conda) {
    msg_return <- suppressWarnings(system2("conda"))
    if (msg_return!=0) {
      stop("conda is not installed in the system, try using pip ee_install_ee(conda=FALSE)")
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
