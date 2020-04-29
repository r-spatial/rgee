---
title: 'rgee: An R package for interacting with Google Earth Engine'
tags:
  - R
  - Earth Engine
  - Earth Observation
  - spatial analysis
authors:
affiliations:
date: \today 
bibliography: paper.bib
---
# Summary

Google Earth Engine (GEE) [@gorelick2017google] is a cloud-based platform specifically
designed for reproducible planetary-scale environmental data analysis. Currently,GEE
is made up of 3 components. The data catalog which is continuously updated and permits 
to users the access to a dataset of over 40 years of satellite imagery for the whole world. 
The Google’s geocomputational infrastructure which use different data distribution models 
depending on the spatial task to perform, reducing mostly the time execution of spatial
non-recursively procedures. Finally, the Web REST API and two client libraries (in JavaScript and Python) 
which permits users to interact with GEE servers without the necessity of understand the complex system architecture and data distributions models behind GEE. At the present day, the client libraries count with more than 800 functions with the possibility of chaining operations.

This paper introduce **rgee**, a new Earth Engine client library. The goal of
**rgee** is to allows users to leverage the strengths of the R spatial ecosystem
and Google Earth Engine in the same worflow. **rgee** import all the existing functionality
of the API and added functions in the high.


The API of `rgee` is created taken into account the expertissness

At different to the Javascript or Python client `rgee` is not a native Earth Engine API.

rgee 
like the Javascript or Python client, to do this would be extremely hard, especially 
considering that the API is in active development. So, how is it possible to run 
Earth Engine using R? the answer is reticulate. reticulate is an R package designed
to allow a seamless interoperability between R and Python. When an Earth Engine request
is created in R, reticulate will transform this piece of code to Python. Once the Python 
code is obtained, the Earth Engine Python API transform the request to a JSON format. 
Finally, the query (in JSON) is received by the Google Earth Engine Platform thanks to a 
Web REST API. The response will follow the same path. If you are searching a way to interact
with the Earth Engine Asset (EEA), rgee offers also functions to batch upload(download)
spatial objects. Additionally, you could easily manage EEA through the ee_manage_* interface.

# Features

## Enhanced I/O 

**rgee** implements several functions to support the download of Images, ImageCollections and FeatureCollections

![Comparison between the Python and R Earth Engine client libraries to transfer data from server to client-side and vice-versa](/home/aybarpc01/rgee-paper/1_rgee_IO.svg)


## Multiple users
## Extraction of time series

```r
library(rgee)
library(sf)

ee_Initialize()

# Define a Image or ImageCollection e.g. Terraclimate
# Mean composite
terraclimate <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
  filterDate("2001-01-01", "2002-01-01")$
  map(function(x) x$select("pr"))$
  mean()$rename("pp_mean") 

# Define a geometry
nc <- st_read(system.file("shape/nc.shp", package = "sf"))

# Extract the average areal rainfall
ee_nc_rain <- ee_extract(terraclimate, nc, sf = TRUE)
```

## Easy visualization

## rgee asset Manage Interface

## Metadata display

# Availability

`rgee` is open source software made available under the GNU GPL-3 license. It can be
installed through CRAN (------) using: install.packages("------").
areal can also be installed from its GitHub repository using the remotes package: remo
tes::install_github("-------").

# References


