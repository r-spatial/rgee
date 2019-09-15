
<img src="https://raw.githubusercontent.com/csaybar/rgee/master/man/figures/logo.png" align="right" width = 15%/>

# Google Earth Engine for R

[![Build
Status](https://travis-ci.org/csaybar/rgee.svg?branch=master)](https://travis-ci.org/csaybar/rgee)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/csaybar/rgee?branch=master&svg=true)](https://ci.appveyor.com/project/csaybar/rgee)
[![Codecov test
coverage](https://codecov.io/gh/csaybar/rgee/branch/master/graph/badge.svg)](https://codecov.io/gh/csaybar/rgee?branch=master)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

[Google Earth Engine](https://earthengine.google.com/) (Gorelick et al.,
2017) is a cloud-based platform that allows users getting access to a
petabyte-scale archive of remote sensing data and run geospatial
analysis on Google’s infrastructure. The `rgee` package provides full
access to the [Google Earth Engine
API](https://developers.google.com/earth-engine/) from within R and
defines additional tools for automating processes.

**Earth Engine Python API**:

``` python
import ee
ee.Initialize()
image = ee.Image('CGIAR/SRTM90_V4')
image.bandNames().getInfo()
#> ['elevation']
```

**rgee:**

``` r
library(rgee)
ee$Initialize()
image <- ee$Image('CGIAR/SRTM90_V4')
image$bandNames()$getInfo()
#> [1] "elevation"
```

## Features

  - [**ee$…:**](https://developers.google.com/earth-engine/) Complete
    access to the Earth Engine API from within
    R.
  - [**ee\_Initialize:**](https://csaybar.github.io/rgee/reference/ee_Initialize.html)
    Multi-user support for Initialize Earth Engine, a wrapper around
    `ee$Initialize`.
  - [**ee\_install:**](https://csaybar.github.io/rgee/reference/ee_check-tools.html)
    Interface for installing Selenium drivers and Python packages
    effortlessly.
  - [**ee\_check:**](https://csaybar.github.io/rgee/reference/ee_check-tools.html)
    Interface for dependencies
    checking.
  - [**ee\_manage:**](https://csaybar.github.io/rgee/reference/ee_manage-tools.html)
    Manage EE assets and tasks recursively.
  - [**ee\_map:**](https://csaybar.github.io/rgee/reference/ee_map.html)
    Display EE spatial objects using
    [mapview](https://r-spatial.github.io/mapview/).
  - [**ee\_print:**](https://csaybar.github.io/rgee/reference/ee_print.html)
    Print EE
    metadata.
  - [**ee\_upload:**](https://csaybar.github.io/rgee/reference/ee_upload.html)
    Upload ESRI shapefiles or GeoTIFF’s to Earth
    Engine.
  - [**ee\_download:**](https://csaybar.github.io/rgee/reference/ee_download.html)
    Download EE spatial
    objects.
  - [**ee\_as\_sf**](https://csaybar.github.io/rgee/reference/ee_upload.html)
    &
    [**sf\_as\_ee:**](https://csaybar.github.io/rgee/reference/ee_download.html)
    Convert EE tables (Feature Collection) to sf and
    vice-versa.
  - [**ee\_as\_thumbnail:**](https://csaybar.github.io/rgee/reference/ee_download.html)
    Create a stars object based on an EE thumbnail
    image.
  - [**ee\_extract:**](https://csaybar.github.io/rgee/reference/ee_upload.html)
    Extract values for EE ImageCollections, similarly to
    [raster::extract](https://www.rdocumentation.org/packages/raster/versions/3.0-2/topics/extract).
  - [**ee\_search:**](https://csaybar.github.io/rgee/reference/ee_search.html)
    Search among the Earth Engine Data
    Catalog.
  - [**ee\_help:**](https://csaybar.github.io/rgee/reference/ee_help.html)
    Display the Earth Engine documentation.

**NOTE: Access to Google Earth Engine is currently only available to
[registered users](https://earthengine.google.com/)**.

## Installation

Install development versions from github with

``` r
remotes::install_git("csaybar/rgee")
```

### Windows

Before install `rgee` be sure that
[Rtools](https://cran.r-project.org/bin/windows/Rtools/) is installed in
the system. The static libraries will automatically downloaded from
[rwinlib](https://github.com/rwinlib/).

### Linux

Please install the follow system libraries.

#### Ubuntu

    sudo add-apt-repository ppa:ubuntugis/ubuntugis-unstable
    sudo apt-get update
    sudo apt-get install libprotobuf-dev protobuf-compiler libv8-3.14-dev libjq-dev libudunits2-dev libproj-dev libgeos-dev libspatialite-dev libgdal-dev libjson-c-dev libnetcdf-dev netcdf-bin

### MacOS

Use [Homebrew](https://brew.sh/) to install system libraries:

    brew install pkg-config
    brew install gdal
    brew install netcdf
    brew install libgit2
    brew install udunits
    brew install curl
    brew install sqlite

### Docker image

Cooming
soon\!

## How does it works?

![workflow](https://raw.githubusercontent.com/csaybar/rgee/master/man/figures/rgee.png)

## Credits

The rgee has been inspired by the following third-party R/Python
packages:

  - **[gdalcubes - Marius
    Appel](https://github.com/appelmar/gdalcubes)**
  - **[gee\_asset\_manager - Lukasz
    Tracewski](https://github.com/tracek/gee_asset_manager)**  
  - **[geeup - Samapriya Roy](https://github.com/samapriya/geeup)**
  - **[geeadd - Samapriya
    Roy](https://github.com/samapriya/gee_asset_manager_addon)**
  - **[cartoee - Kel Markert](https://github.com/KMarkert/cartoee)**
  - **[geetools - Rodrigo E.
    Principe](https://github.com/gee-community/gee_tools)**
  - **[landsat-extract-gee - Loïc
    Dutrieux](https://github.com/loicdtx/landsat-extract-gee)**
  - **[earthEngineGrabR -
    JesJehle](https://github.com/JesJehle/earthEngineGrabR)**

## Contributing

  - `rgee` welcomes any kind of contributions. Please check the [rgee
    Community Code of Conduct]() for details.
  - Are you interested in creating a pull request?. Make sure the code
    follows the [tidyverse style guide](https://style.tidyverse.org/)
    and to submit them to the develop branch of this repository.
  - Please file bug reports and feature requests at
    <https://github.com/csaybar/rgee/issues>.
