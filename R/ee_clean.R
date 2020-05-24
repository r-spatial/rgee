#' Delete Credentials
#'
#' Delete all the credentials according to a specific user. The credentials
#' (Google Earth Engine, Google Drive and Google Cloud Storage) are created
#' after running successfully `ee_Initialize(...)`. They are saved in
#' the path `rgee::ee_get_earthengine_path()`.
#'
#' @param email Character. Earth Engine user (e.g. `data.colec.fbf`).
#' @param quiet Logical. Suppress info messages.
#'
#' @examples
#' \dontrun{
#' library(rgee)
#'
#' ee_clean_credentials()
#' ee_clean_credentials('data.colec.fbf@gmail.com')
#'
#' }
#' @export
ee_clean_credentials <- function(email='not_defined', quiet=FALSE) {
  oauth_func_path <- system.file("python/ee_utils.py", package = "rgee")
  utils_py <- ee_source_python(oauth_func_path)
  ee_path <- ee_utils_py_to_r(utils_py$ee_path())
  email_clean <- gsub("@gmail.com", "", email)

  if (email == 'not_defined') {
    email_clean <- 'ndef'
  }

  path_to_delete <- sprintf("%s/%s", ee_path, email_clean)

  if (!dir.exists(path_to_delete)) {
    cat('The path:', path_to_delete, ' does not exist!\n')
  }

  if (!quiet && dir.exists(path_to_delete)) {
    cat(
      sprintf("Credentials in %s has been deleted.\n",
              sprintf("%s/%s", ee_path, email_clean)))
  }

  unlink(x = path_to_delete, recursive = TRUE, force = TRUE)
  unlink(list.files(ee_path, "@gmail.com", full.names = TRUE))
  unlink(list.files(ee_path, ".json", full.names = TRUE))
  unlink(list.files(ee_path, "credentials", full.names = TRUE))
  unlink(list.files(ee_path, "rgee_sessioninfo.txt", full.names = TRUE))

  invisible(TRUE)
}

#' Remove rgee system variables from .Renviron
#'
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_clean_pyenv()
#' }
#' @export
ee_clean_pyenv <- function() {
  # Read line by line .Renviron
  home <- Sys.getenv("HOME")
  renv <- file.path(home, ".Renviron")
  if (file.exists(renv)) {
    # Backup original .Renviron before doing anything else here.
    file.copy(renv, file.path(home, ".Renviron_backup"))

    con <- file(renv, open = "r")
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
    close(con)

    # Remove system variables EARTHENGINE_PYTHON
    system_vars <- lines[!grepl("EARTHENGINE_PYTHON", lines)]
    fileConn <- file(renv)
    writeLines(system_vars, fileConn)
    close(fileConn)
  }
  invisible(TRUE)
}
