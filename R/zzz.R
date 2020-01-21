.onLoad <- function(libname, pkgname) {
  # use superassignment to update global reference
  ee <<- reticulate::import("ee", delay_load = TRUE)
}

.onAttach <- function(libname, pkgname) {
  #packageStartupMessage("ee is a reserved word for the rgee package, try ee_restart() to reattach")
  options(rgee.print.option = "json")
  #options(rgee.upload.properties = list(time_start="1970-01-01",time_end="1970-01-01"))
}

#' Reattach ee as a reserved word
#' @export
ee_reattach <- function() {
  attached <- search()
  is_rgee_attached <- attached[grepl("rgee", attached)] == 'package:rgee'
  if (is_rgee_attached) {
    detach("package:rgee", unload = TRUE)
    suppressMessages(require(rgee))
  }
}

#' Convert between Python and R objects
#' @param x A python object
#' @export
ee_py_to_r <- function(x) {
  p_r <- suppressWarnings(try(py_to_r(x),silent = TRUE))
  if (class(p_r) %in% 'try-error') {
    return(x)
  } else {
    return(p_r)
  }
}
