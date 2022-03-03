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
#'  \item{4. }{ Set the environment variable EARTHENGINE_PYTHON. It is used to
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
#' @param python_version Only windows users. The version of Python to be used in
#' this conda environment. The associated Python package from conda will be requested
#' as python={python_version}. When NULL, the default python package will be
#' used instead. For example, use python_version = "3.6" to request that the
#' conda environment be created with a copy of Python 3.6. This argument will be
#' ignored if python is specified as part of the packages argument, for backwards
#' compatibility.
#' @param confirm Logical. Confirm before restarting R?.
#' @return No return value, called for installing non-R dependencies.
#' @family ee_install functions
#' @export
ee_install <- function(py_env = "rgee",
                       earthengine_version = ee_version(),
                       python_version = "3.8",
                       confirm = interactive()) {

  #check packages
  ee_check_packages("ee_install", "rstudioapi")

  # If Python not found install miniconda
  if ((!reticulate::py_available(initialize = TRUE))) {
    text <- paste(
      sprintf("%s did not find any Python ENV on your system.",
              bold("reticulate")),
      "",
      bold("Would you like to download and install Miniconda?"),
      "Miniconda is an open source environment management system for Python.",
      "See https://docs.conda.io/en/latest/miniconda.html for more details.",
      sprintf("%s install miniconda/anaconda to use rgee!",
              bold("Windows users must")),
      "",
      "If you think it is an error since you know you have a Python environment",
      "in your system. Run as follow to solve:",
      bold("- Using the rgee API:"),
      "1. rgee::ee_clean_pyenv()",
      "2. rgee::ee_install_set_pyenv(py_path = \"YOUR_PYTHON_PATH_GOES_HERE\")",
      "3. Restart your system.",
      bold("- Using Rstudio 1.4:"),
      "   https://github.com/r-spatial/rgee/tree/help/rstudio/",
      sep = "\n"
    )
    message(text)

    if (confirm) {
      response <- readline("Would you like to install Miniconda? [Y/n]: ")
    } else {
      response <- "y"
    }

    repeat {
      ch <- tolower(substring(response, 1, 1))
      if (ch == "y" || ch == "") {
        reticulate::install_miniconda()
        message(
          "Miniconda was successfully installed, please restart R and run",
          " again rgee::ee_install"
        )
        return(TRUE)
      } else if (ch == "n") {
        message("Installation aborted.")
        return(FALSE)
      } else {
        response <- readline("Please answer yes or no: ")
      }
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
    expr = ee_install_create_pyenv(python_version = python_version, py_env = py_env),
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
  response <- message(
    "\n",
    paste(
      sprintf(
        "%s want to store the environment variables: %s ",
        bold("rgee::ee_install"),
        bold("EARTHENGINE_PYTHON")
      ),
      sprintf(
        "and %s in your %s to use the Python path:",
        bold("EARTHENGINE_ENV"),
        bold(".Renviron file")
      ),
      sprintf("%s in future sessions.", bold(py_path)),
      sep = "\n"
    )
  )

  if (confirm) {
    response <- readline("Would you like to continues? [Y/n]:")
  } else {
    response <- "y"
  }

  repeat {
    ch <- tolower(substring(response, 1, 1))
    if (ch == "y" || ch == "") {
      renv <- ee_install_set_pyenv(py_path = py_path, py_env = py_env, confirm = 3, quiet = TRUE)
      message(
        "\n",
        paste(
          sprintf(
            bold("3. The Environment Variables: 'EARTHENGINE_PYTHON=%s' and 'EARTHENGINE_ENV=%s'"),
            py_path, py_env
          ),
          sprintf("were saved in the .Renviron (%s) file. Remember that you", bold(renv)),
          sprintf(
            "could remove EARTHENGINE_PYTHON and EARTHENGINE_ENV using %s",
            bold("rgee::ee_clean_pyenv().")
          ),
          sep = "\n"
        )
      )
      break
    } else if (ch == "n") {
      message(
        paste(
          "Always that you want to use rgee you will need to run as follow:",
          "----------------------------------",
          "library(rgee)",
          sprintf("Sys.setenv(\"RETICULATE_PYTHON\" = \"%s\")",py_path),
          "ee_Initialize()",
          "----------------------------------",
          "To save the virtual environment \"EARTHENGINE_PYTHON\", run: ",
          sprintf(bold("rgee::ee_install_set_pyenv(py_path = \"%s\")"),py_path),
          sep = "\n"
        )
      )
      break
    } else {
      response <- readline("Please answer yes or no: ")
    }
  }

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
    packages = c("earthengine-api", "numpy"),
    envname = rgee_path
  )

  # Restart to see changes
  if (rstudioapi::isAvailable()) {
    # Restart to see changes
    if (isTRUE(confirm)) {
      title <- paste(
        "",
        bold("Well done! rgee has been successfully installed on your system."),
        sprintf("You need restart R to see changes (%s). After that, we recommend", bold("Windows users must terminate R!")),
        "running ee_check() to perform a full check of all non-R rgee dependencies.",
        "Do you want restart your R session?",
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


#' Configure which version of Python to use with rgee
#'
#' Configure which version of Python to use with rgee. This function creates two
#' environment variables: 'EARTHENGINE_PYTHON' and 'EARTHENGINE_ENV' both will be
#' saved into the file .Renviron.
#'
#' @param py_path The path to a Python interpreter
#' @param py_env The name of the conda or venv environment. If
#' NULL, \code{\link{ee_install_upgrade}} and \code{\link{py_install}} functions will not work.
#' @param Renviron Character. If it is "global" the environment variables are set in
#' the .Renviron located in the Sys.getenv("HOME") folder. On the other hand,  if
#' it is "local" the environment variables are set in the .Renviron on the
#' working directory (getwd()). Finally, users can also enter a specific path
#' (see examples).
#' @param quiet Logical. Suppress info message
#' @param confirm Logical. Confirm before restarting R?.
#' @return no return value, called for setting EARTHENGINE_PYTHON in .Renviron
#' @family ee_install functions
#' @examples
#' \dontrun{
#' library(rgee)
#'
`#' ## For macOS and Linux users
#' # ee_install_set_pyenv(py_path = "/usr/bin/python3", Renviron = "local")
#' # ee_install_set_pyenv(py_path = "/usr/bin/python3", Renviron = "/home/zgis/")
#'
#' ## For Window users
#' # ee_install_set_pyenv(
#' #     py_path = "C://Users/Unicorn/Anaconda3/envs/rgee",
#' #     py_env = "rgee"
#' # )`
#' }
#' @export
ee_install_set_pyenv <- function(py_path,
                                 py_env = NULL,
                                 Renviron = "global",
                                 confirm = interactive(),
                                 quiet = FALSE) {
  if (is.null(py_path) & is.null(py_env) ) {
    stop("Users must defined at least either py_path or py_env.")
  }

  # Get the .Renviron on their system
  if (tolower(Renviron) == "global") {
    home <- Sys.getenv("HOME")
  } else if(tolower(Renviron) == "local") {
    home <- getwd()
  } else {
    if (dir.exists(Renviron)) {
      home <- Renviron
    } else {
      stop(sprintf("The directory %s does not exist!", Renviron))
    }
  }

  renv <- file.path(home, ".Renviron")

  # Backup original .Renviron before doing anything else here.
  if (file.exists(renv)) {
    file.copy(renv, file.path(home, ".Renviron_backup"), overwrite = TRUE)
  }

  # If .Renviron does not exist, create the file.
  if (!file.exists(renv)) {
    file.create(renv)
  }

  if (is.null(py_path)) {
    py_path <- ee_get_python_path(home)
  }

  # Remove rgee environmental variables
  # (EARTHENGINE_PYTHON | EARTHENGINE_ENV) from .Renviron
  ee_clean_pyenv(home)

  # Set EARTHENGINE_PYTHON and EARTHENGINE_ENV in .Renviron.
  python_path <- ee_install_set_pyenv_path(
    py_path = py_path,
    renv = renv
  )

  python_env <- ee_install_set_pyenv_env(
    py_env = py_env,
    py_path = python_path,
    renv = renv ,
    quiet = quiet
  )

  if (!quiet) {
    if (length(python_env) == 0) {
      message(
        sprintf(
          "%s='%s'\nsaved in: %s",
          bold("EARTHENGINE_PYTHON"), python_path,
          bold(renv)
        )
      )
    } else {
      message(
        sprintf(
          "%s='%s'\n%s='%s'\nsaved in: %s",
          bold("EARTHENGINE_PYTHON"), python_path,
          bold("EARTHENGINE_ENV"), python_env,
          bold(renv)
        )
      )
    }
  }


  # Restart to see changes
  if (rstudioapi::isAvailable()) {
    # Restart to see changes
    if (isTRUE(confirm)) {
      title <- sprintf(
        "Do you want restart your R session? %s",
        bold("Windows users could need to terminate R instead of restarting.")
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
  invisible(renv)
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
  on.exit(close(con), add = TRUE)
  invisible(TRUE)
}

#' Create an isolated Python virtual environment to be used in rgee
#' @param py_env The name of, or path to, a Python virtual environment.
#' @importFrom reticulate conda_create virtualenv_create
#' @return Character. The path of the virtual environment created.
#' @noRd
ee_install_create_pyenv <- function(python_version, py_env = "rgee") {
  #Check is Python is greather than 3.5
  ee_check_python(quiet = TRUE)
  if (is_windows()) {
    pyenv_path <- conda_create(py_env, python_version = python_version)
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


#' Upgrade the Earth Engine Python API
#'
#' @param version Character. The Earth Engine Python API version to upgrade.
#' By default \code{rgee::ee_version()}.
#' @param earthengine_env Character. The name, or full path, of the
#' environment in which the earthengine-api packages are to be installed.
#' @return no return value, called to upgrade the earthengine-api Python package
#' @family ee_install functions
#' @export
ee_install_upgrade <- function(version = NULL,
                               earthengine_env = Sys.getenv("EARTHENGINE_ENV")) {
  if (earthengine_env == "") {
    stop(
      "ee_install_upgrade needs to define EARTHENGINE_ENV.\n",
      "Run ee_install_set_pyenv(py_env = \"YOUR_ENV\")",
      " to set a Python environment."
      )
  }
  if (is.null(version)) {
    version <- rgee::ee_version()
  }
  reticulate::py_install(
    packages = c(sprintf("earthengine-api==%s", version)),
    envname = earthengine_env
  )
  title <- paste(
    "",
    bold(
      sprintf(
        "Well done! the Earth Engine Python API was successfully upgraded (%s).",
        version
      )
    ),
    "rgee needs restart R to see changes.",
    "Do you want to continues?",
    sep = "\n"
  )
  response <- menu(c("yes", "no"), title = title)
  switch(response + 1,
         cat("Restart R session to see changes.\n"),
         rstudioapi::restartSession(),
         cat("Restart R session to see changes.\n"))
}


#' Search if EARTHENGINE_INIT_MESSAGE is set
#' @noRd
ee_search_init_message <- function() {
  home <- Sys.getenv("HOME")
  renv <- file.path(home, ".Renviron")
  if (!file.exists(renv)) {
    return(FALSE)
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
  on.exit(close(con), add = TRUE)
  # Find if EARTHENGINE_INIT_MESSAGE is set
  any(grepl("EARTHENGINE_INIT_MESSAGE", lines))
}

#' Set a Python PATH
#' @noRd
ee_install_set_pyenv_path <- function(py_path, renv) {
  # Open & read .Renviron
  con <- file(renv, open = "r+")
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

  to_remote <- NULL
  if (!is.null(py_path)) {
    ret_python <- sprintf('EARTHENGINE_PYTHON="%s"', py_path)
    to_remote <- c(to_remote, ret_python)
  }
  system_vars <- c(lines, to_remote)
  writeLines(system_vars, con)
  on.exit(close(con), add = TRUE)
  # return the python path
  ee_p <- system_vars[grepl("EARTHENGINE_PYTHON=", system_vars)]
  gsub("EARTHENGINE_PYTHON=|\"","",ee_p)
}

#' Set a Python ENV
#' @noRd
ee_install_set_pyenv_env <- function(py_env, py_path, renv, quiet) {
  # Open & read .Renviron
  con <- file(renv, open = "r+")
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

  to_remote <- NULL
  if (!is.null(py_env)) {
    ret_env <- sprintf('EARTHENGINE_ENV="%s"', py_env)
    to_remote <- c(to_remote, ret_env)
    if (length(py_path) == 0) {
      stop(
        "Impossible to set a Python ENV without a Python PATH. py_path can not be NULL."
      )
    }
  } else {
    if (is_windows()) {
      stop_message <- sprintf(
        "%s install miniconda/anaconda to use rgee. %s %s",
        bold("Windows users must"),
        "The use of a Python environment is",
        bold("mandatory.")
      )
      stop(stop_message)
    } else {

      text_msg <- paste(
        sprintf(
          "rgee will work if the Python PATH: %s", bold(py_path)
        ),
        sprintf(
          "has installed the Python packages %s and %s", bold("earth-engine"), bold("numpy.")
        ),
        "The following functions will not work until you set a ",
        sprintf("Python environment: %s and %s.\n",
                bold("rgee::ee_install_upgrade"),
                bold("reticulate::py_install")),
        sep = "\n"
      )
      if (!quiet) {
        message(text_msg)
      }
    }
  }
  system_vars <- c(lines, to_remote)
  writeLines(system_vars, con)
  on.exit(close(con), add = TRUE)
  # return the python path
  ee_p <- system_vars[grepl("EARTHENGINE_ENV=", system_vars)]
  gsub("EARTHENGINE_ENV=|\"","",ee_p)
}


#' Set a Python ENV
#' @noRd
ee_get_python_path <- function(home) {
  renv <- file.path(home, ".Renviron")
  if (!file.exists(renv)) {
    return(FALSE)
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
  on.exit(close(con), add = TRUE)
  # Find if EARTHENGINE_INIT_MESSAGE is set
  gsub("EARTHENGINE_PYTHON=|\"", "", lines[grepl("EARTHENGINE_PYTHON", lines)])
}
