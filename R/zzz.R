.onLoad <- function(libname, pkgname) {
  # use superassignment to update global reference
  ee <<- reticulate::import("ee", delay_load = FALSE)
  reticulate::configure_environment(pkgname)
}

.onAttach <- function(libname, pkgname) {
  #packageStartupMessage(
  options(rgee.print.option = "simply")
}

#' Reattach ee as a reserved word
#' @export
ee_reattach <- function() {
  ee <<- reticulate::import("ee", delay_load = TRUE)
}
