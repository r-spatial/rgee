library(testthat)
library(rgee)
library(reticulate)
library(raster)
library(stars)
library(sf)

ee_reattach()
ee_Initialize()

test_check("rgee")
