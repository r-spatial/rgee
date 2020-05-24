.onAttach <- function(libname, pkgname) {
  #packageStartupMessage(
  options(rgee.print.option = "simply")
}

.onLoad <- function(libname, pkgname) {
  # Configure a Python environment
  reticulate::configure_environment(pkgname)

  # if EARTHENGINE_PYTHON is defined then forward it to RETICULATE_PYTHON
  earthengine_python <- Sys.getenv("EARTHENGINE_PYTHON", unset = NA)
  if (!is.na(earthengine_python))
    Sys.setenv(RETICULATE_PYTHON = earthengine_python)

  ee <<- reticulate::import("ee", delay_load = TRUE)
}


#' Reattach ee as a reserved word
#' @export
ee_reattach <- function() {
  ee <<- reticulate::import("ee", delay_load = TRUE)
}
