.onAttach <- function(libname, pkgname) {
  options(rgee.print.option = "simply")
}

.onLoad <- function(libname, pkgname) {
  # # if EARTHENGINE_PYTHON is defined then forward it to RETICULATE_PYTHON
  # earthengine_python <- Sys.getenv("EARTHENGINE_PYTHON", unset = NA)
  # if (!is.na(earthengine_python))
  #   Sys.setenv(RETICULATE_PYTHON = earthengine_python)

  # delay load earthengine-api
  ee <<- reticulate::import("ee", delay_load = TRUE)
}
