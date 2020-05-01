---
title: 'rgee: An R package for interacting with Google Earth Engine'
bibliography: paper.bib
date: \today
output: pdf_document
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
---

# Summary
Google Earth Engine (GEE) [@gorelick2017google] is a cloud-based platform specifically designed for planetary-scale environmental data analysis. Currently, GEE is made up of 3 components. The data catalog which is continuously updated and permits users to access a dataset of over 40 years of satellite imagery for the whole world.  The Google’s geocomputational infrastructure highly optimized to reduce the time execution of spatial non-recursively procedures. Finally, the Web REST API and the two client libraries (in JavaScript and Python) permits users to interact with the server-side without the necessity to understand the complex system architecture and data distributions models behind GEE. Although the GEE functionality is powerful with more than 800 functions, and the possibility of chaining operations,  there are limitations to creating straightforward input/output pipelines, quality static visualization, metadata display, and efficient management of Earth Engine asset resources. This becomes a more challenging task outside the Python Earth Engine API [@markert2019cartoee].

This paper introduces **rgee**, an Earth Engine client library for R. All the classes and the existing functionality of the two Google's supported client libraries can be called through the dollar 
sign (`$`). `rgee` adds several new features such as (i) new I/O design, (ii) multiple user support, (iii) easily extraction of time series and zonal statistics, (iv) asset manage interface, and (v) metadata display, also with `rgee` is possible the execution of Earth Engine Python code from within R which make the translation of large Python projects unnecessary. The goal of **rgee** is to allows users to leverage the strengths of the R spatial ecosystem and Google Earth Engine in the same workflow.

# Features

## I/O Enhanced

**rgee** implements several functions to support the download/upload of image and vector datasets (Table 1 and Figure 1). For instance, to download images located in the server-side you might use either `ee_image_as_raster` or `ee_image_as_stars`. All the download functions implemented in `rgee` have the option to download via using an intermediate container (Google Drive or Google Cloud Storage) or a REST call ("$getInfo"). Although the last option permits users a direct download, there is a limitation of 262144 pixels (for images) or 5000 elements (for featurecollections) by request which makes it not recommendable for large objects. The upload process follows the same path. In **rgee** we implement `raster_as_ee`, `stars_as_ee` for upload image and `sf_as_ee` for vector data. Large uploads are just possible through a Google Cloud Storage account active. 

![Comparison between the Python and R Earth Engine client libraries to transfer data from server to client-side and vice-versa](/home/aybarpc01/Github/rgee/1_rgee_IO.svg)

|          	|         	|                   	|         from         	|          to          	|            return           	|
|:--------:	|---------	|-------------------	|:--------------------:	|:--------------------:	|:---------------------------:	|
| Download 	| Image   	| ee_image_to_drive 	|    EE server-side    	|     Google Drive     	|        Unstarted task       	|
|          	|         	| ee_image_to_gcs   	|    EE server-side    	| Google Cloud Storage 	|        Unstarted task       	|
|          	|         	| ee_image_to_asset 	|    EE server-side    	|       EE asset       	|        Unstarted task       	|
|          	|         	| ee_as_raster      	|    EE server-side    	|         Local        	|      RasterStack object     	|
|          	|         	| ee_as_stars       	|    EE server-side    	|         Local        	|      Proxy-stars object     	|
|          	| Table   	| ee_table_to_drive 	|    EE server-side    	|     Google Drive     	|        Unstarted task       	|
|          	|         	| ee_table_to_gcs   	|    EE server-side    	| Google Cloud Storage 	|        Unstarted task       	|
|          	|         	| ee_table_to_asset 	|    EE server-side    	|       EE asset       	|        Unstarted task       	|
|          	|         	| ee_as_sf          	|    EE server-side    	|         Local        	|          sf object          	|
|          	| Generic 	| ee_drive_to_local 	|     Google Drive     	|         Local        	|       object filename       	|
|          	|         	| ee_gcs_to_local   	| Google Cloud Storage 	|         Local        	|       object filename       	|
| Upload   	| Images  	| gcs_to_ee_image   	| Google Cloud Storage 	|       EE asset       	|       ee.Image object       	|
|          	|         	| raster_as_ee      	|         Local        	|       EE asset       	|       ee.Image object       	|
|          	|         	| stars_as_ee       	|         Local        	|       EE asset       	|       ee.Image object       	|
|          	| Table   	| gcs_to_ee_table   	| Google Cloud Storage 	|       EE asset       	| ee.FeatureCollection object 	|
|          	|         	| sf_as_ee          	|         Local        	|       EE asset       	| ee.FeatureCollection object 	|
|          	| Generic 	| local_to_gcs      	|         Local        	| Google Cloud Storage 	|         GCS filename        	|

## Multiple users
`rgee` ofrece la posibilidad de manejar credenciales de Earth Engine, Google Drive y Google Cloud Storage para multiples usuarios. Estos posibilita que equipos de trabajo paralelizen sus procesos tanto al lado del servidor como al lado del cliente. Por ejemplo, al analizar la deforestacion un grupo de investigadores podria crear un script de la siguiente manera:

```r
library(rgee)
gmails <-  c("csaybar", "ryali93", "lbautista")

for (gmail in gmails) {
  ee_Initialize(gmail)  
  ic_results <- temporal_deforestation(split = gmail, ...) 
  ee_imagecollection_to_local(ic_results)
}
```

## Extraction of time series
`rgee` can extract values from `ee.Image` and `ee.ImageCollection` at user-defined sf object or vector Earth Engine objects. Users can summarize the values considering built-in EE reducer functions that return one value. 

```r
library(rgee)
library(sf)

ee_Initialize()

# Image or ImageCollection (mean composite)
terraclimate <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
  filterDate("2001-01-01", "2002-01-01")$
  map(function(x) x$select("pr"))$
  mean()$rename("pp_mean") 

# Define a geometry
nc <- st_read(system.file("shape/nc.shp", package = "sf"))

# Extract the average areal rainfall
ee_nc_rain <- ee_extract(terraclimate, nc, sf = TRUE)
```

## Asset Manage Interface
`rgee` inspired in previous works [@samapriya_roy_2020_3772053] implement an interface to batch actions on assets. Users can access 
to the interface through the serie of ee_manage_* functions. In `rgee`, we implement functions to create and delete folders, moving and copy assets, set and delete properties in assets, access control lists, and manage tasks. This interface extending capabilities of existing GEE CLI (ee.data.*).

## Metadata display

Fetch and return metadata info about 



# Availability
`rgee` is open source software made available under the Apache v2 license. It can be
installed through CRAN (------) using: install.packages("------").
`rgee` can also be installed from its GitHub repository using the remotes package: remo
tes::install_github("-------").

# References


