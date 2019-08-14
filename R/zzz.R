.onLoad <- function(libname, pkgname) {
  # use superassignment to update global reference to scipy
  ee <<- reticulate::import("ee", delay_load = TRUE)
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("ee is a reserved word for the rgee package, try ee_ee() to reattach")
  options(rgee.print.option = "json")
}

#' Reattach ee as a reserved word
#' @export
ee_ee <- function() {
  attached <- search()
  is_rgee_attached <- length(attached[grepl("rgee", attached)])
  if (is_rgee_attached) {
    detach("package:rgee", unload = TRUE)
    require(rgee)
  }
}


