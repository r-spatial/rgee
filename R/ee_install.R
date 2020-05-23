#' Create an isolated Python virtual environment with all rgee dependencies.
#'
#' @param py_env Character. The name, or full path, of the Python environment
#' to be used by rgee. If it does not exist, a new Python environment will be
#' created.
#' @param confirm Logical. Confirm if restart R when the 'install'
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_install() #It is just necessary once
#' ee_Initialize()
#' }
#' @export
ee_install <- function(py_env = "rgee", confirm = interactive()) {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    stop("package rstudioapi required, please install it first")
  }
  # If Python not found install miniconda
  if (!reticulate::py_available(initialize = TRUE)) {
    text <- paste(
      "No non-system installation of Python could be found.",
      "Would you like to download and install Miniconda?",
      "Miniconda is an open source environment management system for Python.",
      "See https://docs.conda.io/en/latest/miniconda.html for more details.",
      "",
      sep = "\n"
    )
    message(text)
    response <- readline("Would you like to install Miniconda? [Y/n]: ")
    repeat {
      ch <- tolower(substring(response, 1, 1))
      if (ch == "y" || ch == "") {
        reticulate::install_miniconda()
        return(TRUE)
      }
      if (ch == "n") {
        message("Installation aborted.")
        return(FALSE)
      }
      response <- readline("Please answer yes or no: ")
    }
  }

  # Create a python environment
  message(sprintf("1. Creating a Python Environment (%s)", py_env))
  rgee_path <- tryCatch(
    expr = ee_install_create_pyenv(py_env = py_env),
    error = function(e) stop(
      "An error occur when ee_install was creating the Python Environment. ",
      "Run ee_clean_pyenv() and Restart the R session, before trying again."
    )
  )

  # Find the Python Path of the environment created
  if (is_windows()) {
    # In windows, conda_create return the Python executable (.../python.exe)
    # rather than in linux and MacOS that return the path of the Environment
    # (a folder!!). It is a little tricky, maybe it changes on future version of
    # reticulate.
    py_path <- rgee_path # py_path --> Python executable
    rgee_path <- dirname(py_path) # rgee_path --> Env path
  } else {
    # List Python Path in rgee
    python_files <- list.files(
      path = rgee_path,
      pattern =  "python",
      recursive = TRUE,
      full.names = TRUE
    )
    py_path <- python_files[grepl("^python$", basename(python_files))][1]
  }

  # Stop if py_path is not found
  if (length(py_path) == 0) {
    stop(
      "Imposible to find a Python virtual environment. Try to install",
      " the Earth Engine Python API manually "
    )
  }

  # Create EARTHENGINE_PYTHON
  message(
    sprintf("2. The Environment Variable 'EARTHENGINE_PYTHON=%s' ", py_path),
    "was set on the .Renviron file. Remember that you can remove it using",
    " reticulate::ee_clean_pyenv()."
  )

  ee_install_set_pyenv(py_path = py_path)

  # Install the Earth Engine API
  message("3. Installing the earthengine-api. Running ...")
  message(sprintf("reticulate::py_install(packages = 'earthengine-api', envname = '%s')", rgee_path))
  tryCatch(
    expr = ee$computedobject,
    error = function(e) reticulate::py_install(
      packages = c("earthengine-api", "numpy"),
      envname = rgee_path
    )
  )

  # Restart to see changes
  if (rstudioapi::isAvailable()) {
    # Restart to see changes
    if (isTRUE(confirm)) {
      title <- paste0(
        "rgee needs restart R to see changes.\n",
        "Do you want to continues?"
      )
      response <- menu(c("yes", "no"), title = title)
    } else {
      response <- confirm
    }
    switch(response + 1,
           cat("Restart R session to see changes.\n"),
           rstudioapi::restartSession())
  } else {
    message("rgee needs to restart the R session to see changes.\n")
  }
  invisible(TRUE)
}


#' Create an isolated Python virtual environment to be used in rgee
#' @param py_env The name of, or path to, a Python virtual environment.
#' @importFrom reticulate conda_create virtualenv_create
#' @return Character. The path of the virtual environment created.
#' @noRd
ee_install_create_pyenv <- function(py_env = "rgee") {
  #Check is Python is greather than 3.5
  ee_check_python(quiet = TRUE)
  if (is_windows()) {
    pyenv_path <- conda_create(py_env)
  } else {
    pyenv_path <- virtualenv_create(py_env)
  }
  pyenv_path
}

#' Set the Python environment to be used on rgee
#'
#' @param py_path The path to a Python interpreter, to be used with rgee.
#' @noRd
ee_install_set_pyenv <- function(py_path) {
  ee_clean_pyenv()
  # Trying to get the env from the py_path
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

  # Set EARTHENGINE_PYTHON in .Renviron
  ret_python <- sprintf('EARTHENGINE_PYTHON="%s"', py_path)
  system_vars <- c(lines, ret_python)

  writeLines(system_vars, con)
  close(con)
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
