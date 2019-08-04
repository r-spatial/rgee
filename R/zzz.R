.onLoad <- function(libname, pkgname) {
  # use superassignment to update global reference to scipy
  ee <<- reticulate::import("ee", delay_load = TRUE)
}
