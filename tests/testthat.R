library(testthat)
library(rgee)
library(reticulate)
library(raster)
library(stars)
library(sf)

ee_Initialize()

test_check("rgee")
