ops <- options("crayon.enabled" = FALSE, warn = 1)

library(testthat)
library(rgee)
library("covr")

test_check("rgee")
options(ops)
