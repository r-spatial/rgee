#' Delete Credentials
#'
#' Delete all the credentials according to a specific user. The credentials
#' are saved in the path `rgee::ee_get_earthengine_path()` after running
#' successfully at least once  `ee_Initialize(...)`. However, if you run
#' `ee_Initialize(...)` with the same email argument the new credentials
#' will be overwritten.
#' @param email Character (optional, e.g. `pinkiepie@gmail.com`).
#' The directory (all user credentials) to delete.
#' @param quiet Logical (optional). Suppress info messages.
#' @examples {
#' ee_remove_credentials()
#' ee_remove_credentials('pinkiepie')
#' ee_remove_credentials('`pinkiepie@gmail.com`')
#' }
#' @export
ee_remove_credentials <- function(email='not_defined', quiet=FALSE) {
  ee_path <- path.expand("~/.config/earthengine")
  email_clean <- gsub("@gmail.com", "", email)
  if (email == 'not_defined') {
    email_clean = 'ndef'
  }
  path_to_delete <- sprintf("%s/%s", ee_path, email_clean)

  if (!dir.exists(path_to_delete)) {
    stop('The path:', path_to_delete, ' does not exist!')
  }

  unlink(x = path_to_delete, recursive = TRUE, force = TRUE)
  unlink(list.files(ee_path, "@gmail.com", full.names = TRUE))
  unlink(list.files(ee_path, ".json", full.names = TRUE))
  unlink(list.files(ee_path, "credentials", full.names = TRUE))
  unlink(list.files(ee_path, "rgee_sessioninfo.txt", full.names = TRUE))
  if (!quiet) {
    cat(sprintf("Credentials in %s has been deleted.", sprintf("%s/%s", ee_path, email_clean)))
  }

  invisible(TRUE)
}


#' Delete chromedriver
#'
#' @param quiet Logical (optional). Suppress info messages.
#' @examples {
#' ee_remove_driver()
#' }
#' @export
ee_remove_driver <- function(quiet = FALSE) {
  ee_path <- path.expand("~/.config/earthengine/")
  gecko_driver_linux <- sprintf("%s/chromedriver", ee_path)
  gecko_driver_win <- sprintf("%s/chromedriver.exe", ee_path)
  if (file.exists(gecko_driver_win)) file.remove(gecko_driver_win)
  if (file.exists(gecko_driver_linux)) file.remove(gecko_driver_linux)
  if (!quiet) {
    cat(sprintf("GoogleDrive driver in %s has been removed.", ee_path))
  }
  invisible(TRUE)
}
