ops <- options("crayon.enabled" = FALSE, warn = 1)

library(testthat)
library(rgee)
library("covr")

# Skip tests on Solaris as gcc is not in the PATH and I do not have an easy way
# to mimic the CRAN build environment
if (!tolower(Sys.info()[["sysname"]]) == "sunos") {
  Sys.setenv("R_TESTS" = "")
  test_check("rgee")
}

options(ops)
