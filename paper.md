---
title: 'rgee: An R package for interacting with Google Earth Engine'
tags:
  - R
  - Earth Engine
  - Earth Observation
  - spatial analysis
authors:
  - Cesar Luis Aybar Camacho
  - Justin Braaten
  - Qiusheng Wu
  - Lesly Aracelly Bautista
  - Roy Yali Samaniego
affiliations:

date: \today 
bibliography: paper.bib
---
# Summary

Google Earth Engine (GEE) [@gorelick2017google] is a cloud-based platform specifically designed for reproducible planetary-scale environmental data analysis. Currently, GEE is made up of 3 components. The data catalog which is continuously updated and permits users to access a dataset of over 40 years of satellite imagery for the whole world.  The Google’s geocomputational infrastructure, highly optimized, reducing mostly the time execution of spatial non-recursively procedures. Finally, the Web REST API and the two client libraries (in JavaScript and Python) which permits users to interact with the server-side without the necessity of understanding the complex system architecture and data distributions models behind GEE. Although the GEE functionality is powerful with more than 800 functions, and the possibility of chaining operations,  there are limitations to creating straightforward input/output pipelines, quality static visualization, metadata display, and efficient management of Earth Engine asset resources. This becomes a more challenging task outside the Python Earth Engine API [@markert2019cartoee].

This paper introduces **rgee**, an Earth Engine client library for R. GEE classes can be instantiated from R and all the existing functionality of the two Google's supported client libraries can be called through the dollar sign (`$`). `rgee` adds several new features to the existing offering for the native client libraries such as (i) new I/O design, (ii) multiple user support, (iii) Easily extraction of time series and zonal statistics, (iv) Asset Manage Interface, and (v) metadata display, also with `rgee` is possible the execution of Earth Engine Python code from within R which make the translation between R and Python unnecessary. The goal of **rgee** is to allows users to leverage the strengths of the R spatial ecosystem and Google Earth Engine in the same workflow.

# Features

## Enhanced I/O 

**rgee** implements several functions to support the download/upload of image and vector datasets (Table 1 and Figure 1). For instance, to download images in the server-side you might use either `ee_image_as_raster` or `ee_image_as_stars` and for Earth Engine vector data `ee_as_sf`. All the download functions implemented have the option to use an intermediate container (Google Drive or Google Cloud Storage) or a REST call ("$getInfo") which retrieves all the information about the object. Although the last option permits users a direct download, there is a limitation of 262144 pixels (for images) or 5000 elements (for featurecollections) by request which makes it not recommendable for large objects. The upload process follows the same path. In **rgee** we implement `raster_as_ee`, `stars_as_ee` for upload image and `sf_as_ee` for vector data. Large uploads are just possible through the use of a Google Cloud Storage as an intermediate container. 

![Comparison between the Python and R Earth Engine client libraries to transfer data from server to client-side and vice-versa](/home/aybarpc01/Github/rgee/1_rgee_IO.svg)


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

## rgee asset Manage Interface

## Metadata display

# Availability

`rgee` is open source software made available under the Apache v2 license. It can be
installed through CRAN (------) using: install.packages("------").
`rgee` can also be installed from its GitHub repository using the remotes package: remo
tes::install_github("-------").

# References


