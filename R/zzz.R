.onLoad <- function(libname, pkgname) {
  # use superassignment to update global reference
  ee <<- reticulate::import("ee", delay_load = TRUE)
}

.onAttach <- function(libname, pkgname) {
  #packageStartupMessage(
  #"ee is a reserved word for the rgee package, try ee_restart() to reattach")
  options(rgee.print.option = "simply")
  #options(rgee.upload.properties = list(time_start="1970-01-01",
  #                                      time_end="1970-01-01"))
}

#' Reattach ee as a reserved word
#' @export
ee_reattach <- function() {
  ee <<- reticulate::import("ee", delay_load = TRUE)
}
