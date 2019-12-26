
<img src="https://raw.githubusercontent.com/csaybar/rgee/master/man/figures/logo.png" align="right" width = 15%/>

# Google Earth Engine for R

[![Build
Status](https://travis-ci.org/csaybar/rgee.svg?branch=dev)](https://travis-ci.org/csaybar/rgee)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/csaybar/rgee?branch=master&svg=true)](https://ci.appveyor.com/project/csaybar/rgee)
[![codecov](https://codecov.io/gh/csaybar/rgee/branch/master/graph/badge.svg)](https://codecov.io/gh/csaybar/rgee)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CII Best
Practices](https://bestpractices.coreinfrastructure.org/projects/3527/badge)](https://bestpractices.coreinfrastructure.org/projects/3527)
[![DOI](https://zenodo.org/badge/205997187.svg)](https://zenodo.org/badge/latestdoi/205997187)

`rgee` is a bindings package for calling [Google
Earth Engine API](https://developers.google.com/earth-engine/) from
within R. Additionally, functions have been done for making painless 
the connection with the R spatial ecosystem.

## What is Google Earth Engine?

[Google Earth Engine](https://earthengine.google.com/) is a cloud-based
platform that allows users getting access to a petabyte-scale archive of
remote sensing data and run geospatial analysis on Google’s
infrastructure. Google currently just offers Python and JavaScript support.

**Earth Engine Python API**:

``` python
import ee
ee.Initialize()
image = ee.Image('CGIAR/SRTM90_V4')
image.bandNames().getInfo()
#> [u'elevation']
```

**rgee:**

``` r
library(rgee)
ee$Initialize()
image <- ee$Image('CGIAR/SRTM90_V4')
image$bandNames()$getInfo()
#> [1] "elevation"
```
## Installation

To get started, install the gee R package from GitHub as follows:

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

### Docker image (Recommended way to use rgee)

    docker run -d -p 8787:8787 -e USER=rgee -e PASSWORD=rgee --name rgee-dev csaybar/rgee

After that, in your preferred browser, run:

    127.0.0.1:8787

## Features

The main user relevant functions are:

  - [**ee$…:**](https://developers.google.com/earth-engine/) Complete access to the Earth Engine API from within R.
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

## How does it works?

![workflow](https://raw.githubusercontent.com/csaybar/rgee/master/man/figures/rgee.png)

## Code of Conduct

Please note that the `rgee` project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.

## Contributing Guide

👍🎉 First off, thanks for taking the time to contribute\! 🎉👍 Please review our [Contributing Guide](CONTRIBUTING.md).


## Share the love ❤️

Think **rgee** is useful? Let others discover it, by telling them in
person, via Twitter or a blog post.

Using **rgee** for a paper you are writing? Consider citing it

``` r
citation("rgee")
#> 
#> To cite dbparser in publications use:
#> 
#>   Aybar Cesar, Yali Roy, Barja Antony, and Maria Aduato (2019). rgee: Google Earth Engine for R.
#>   R package version 0.2.0.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {Google Earth Engine for R},
#>     author = {Aybar Cesar, Yali Roy, Barja Antony, and Maria Aduato},
#>     organization = {},
#>     year = {2019},
#>     note = {R package version 0.2.0},
#>     url = {https://CRAN.R-project.org/package=rgee},
#>   }
```
