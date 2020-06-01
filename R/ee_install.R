#' Create an isolated Python virtual environment with all rgee dependencies.
#'
#' Create an isolated Python virtual environment with all rgee dependencies.
#' \code{ee_install} realize the following six (6) tasks:
#' \itemize{
#'  \item{1. }{If you do not count with a Python environment, it will display
#'  an interactive menu to install [Miniconda](https://docs.conda.io/en/latest/miniconda.html)
#'  (a free minimal installer for conda).}
#'  \item{2. }{Remove the previous Python environment defined in \code{py_env} if
#'  it exist.}
#'  \item{3. }{Create a new Python environment (See \code{py_env}).}
#'  \item{4. }{ Set the environmental variable EARTHENGINE_PYTHON. It is used to
#'  define RETICULATE_PYTHON when the library is loaded. See this
#'  \href{https://rstudio.github.io/reticulate/articles/versions.html}{article}
#'  for further details.
#'  }
#'  \item{5. }{Install rgee Python dependencies. Using
#'  \code{reticulate::py_install}.}
#'  \item{6. }{Interactive menu to confirm if restart the R session to see
#'  changes.}
#' }
#'
#' @param py_env Character. The name, or full path, of the Python environment
#' to be used by rgee.
#' @param earthengine_version Character. The Earth Engine Python API version
#' to install. By default \code{rgee::ee_version()}.
#' @param confirm Logical. Confirm before restarting R?.
#' @family ee_install functions
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_install() #It is just necessary once
#' }
#' @export
ee_install <- function(py_env = "rgee",
                       earthengine_version = ee_version(),
                       confirm = interactive()) {
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
      "If you think it is an error since you previously created a Python",
      sprintf(
        "environment in the system. Run %s to remove rgee",
        bold('rgee::ee_clean_pyenv()')
      ),
      "environmental variables. After this, restart the R session and try",
      "again.",
      "",
      sep = "\n"
    )
    message(text)
    response <- readline("Would you like to install Miniconda? [Y/n]: ")
    repeat {
      ch <- tolower(substring(response, 1, 1))
      if (ch == "y" || ch == "") {
        reticulate::install_miniconda()
      }
      if (ch == "n") {
        message("Installation aborted.")
        return(FALSE)
      }
      response <- readline("Please answer yes or no: ")
    }
  }

  # Print your current Python config
  cat(
    rule(
      right = bold(
        sprintf(
          "Python configuration used to create %s",
          py_env
        )
      )
    )
  )
  cat("\n")
  print(reticulate::py_config())
  cat(rule(), "\n")

  # Create a python environment
  message(
    bold(
      sprintf(
        "1. Removing the previous Python Environment (%s), if it exists ...",
        py_env
      )
    )
  )
  try_error <- try(ee_install_delete_pyenv(py_env), silent = TRUE)
  if (class(try_error) == "try-error") {
    message(sprintf("%s not found \n", py_env))
  }

  message("\n", bold(sprintf("2. Creating a Python Environment (%s)", py_env)))
  rgee_path <- tryCatch(
    expr = ee_install_create_pyenv(py_env = py_env),
    error = function(e) stop(
      "An error occur when ee_install was creating the Python Environment. ",
      "Run ee_clean_pyenv() and restart the R session, before trying again."
    )
  )

  # Find the Python Path of the environment created
  if (is_windows()) {
    # conda_create returns the Python executable (.../python.exe)
    # rather than in linux and MacOS that returns the path of the Environment
    # (a folder!!). It is a little tricky, maybe it changes on future version
    # of reticulate.
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
    py_path <- python_files[grepl("^python", basename(python_files))][1]
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
    "\n",
    sprintf(
      bold("3. The Environment Variable 'EARTHENGINE_PYTHON=%s' "),
      py_path
    ),
    "was set on the .Renviron file. Remember that you can remove it using",
    " rgee::ee_clean_pyenv()."
  )

  ee_install_set_pyenv(py_path = py_path)

  # Install the Earth Engine API
  message("\n", bold("4. Installing the earthengine-api. Running: "))
  message(
    sprintf(
      "reticulate::py_install(packages = 'earthengine-api', envname = '%s')",
      rgee_path
    ),
    "\n"
  )

  reticulate::py_install(
      packages = c(
        sprintf("earthengine-api==%s", earthengine_version),
        "numpy"
      ),
      envname = rgee_path
  )

  # Restart to see changes
  if (rstudioapi::isAvailable()) {
    # Restart to see changes
    if (isTRUE(confirm)) {
      title <- paste(
        "",
        bold("Well done! rgee was successfully set up in your system."),
        "rgee needs restart R to see changes.",
        "Do you want to continues?",
        sep = "\n"
      )
      response <- menu(c("yes", "no"), title = title)
    } else {
      response <- confirm
    }
    switch(response + 1,
           cat("Restart R session to see changes.\n"),
           rstudioapi::restartSession(),
           cat("Restart R session to see changes.\n"))
  } else {
    message("rgee needs to restart the R session to see changes.\n")
  }
  invisible(TRUE)
}

#' Set the Python environment to be used by rgee
#'
#' This function create a new environment variable called 'EARTHENGINE_PYTHON'.
#' It is used to set the Python environment to be used by rgee.
#' EARTHENGINE_PYTHON is saved into the file .Renviron.
#'
#' @param py_path The path to a Python interpreter.
#' @family ee_install functions
#' @export
ee_install_set_pyenv <- function(py_path) {
  ee_clean_pyenv()
  # Trying to get the env from the py_path
  home <- Sys.getenv("HOME")
  renv <- file.path(home, ".Renviron")

  if (file.exists(renv)) {
    # Backup original .Renviron before doing anything else here.
    file.copy(renv, file.path(home, ".Renviron_backup"), overwrite = TRUE)
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

#' Set EARTHENGINE_INIT_MESSAGE as an environment variable
#' @noRd
ee_install_set_init_message <- function() {
  ee_clean_message()
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
  ret_python <- sprintf('EARTHENGINE_INIT_MESSAGE="%s"', "True")
  system_vars <- c(lines, ret_python)

  writeLines(system_vars, con)
  close(con)
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

#' Delete an isolated Python virtual environment to be used in rgee
#' @param py_env The name of, or path to, a Python virtual environment.
#' @importFrom reticulate conda_remove virtualenv_remove
#' @return Character. The path of the virtual environment created.
#' @noRd
ee_install_delete_pyenv <- function(py_env = "rgee") {
  #Check is Python is greather than 3.5
  ee_check_python(quiet = TRUE)
  if (is_windows()) {
    try(conda_remove(py_env), silent = TRUE)
  } else {
    try(virtualenv_remove(py_env, confirm = FALSE), silent = TRUE)
  }
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

