.onLoad <- function(libname, pkgname) {
  # use superassignment to update global reference to scipy
  cat("Consider that ee is a reserved word for the rgee packages\n")
  ee <<- reticulate::import("ee", delay_load = TRUE)
}
