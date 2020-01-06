---
output:
  html_document: default
  pdf_document: default
---

<img src="https://raw.githubusercontent.com/csaybar/rgee/master/man/figures/logo.png" align="right" width = 15%/>

# Google Earth Engine for R

**NOTE: Access to Google Earth Engine is currently only available to
[registered users](https://earthengine.google.com/)**.

[![Build
Status](https://travis-ci.org/csaybar/rgee.svg?branch=master)](https://travis-ci.org/csaybar/rgee)
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
[![CRAN
status](https://www.r-pkg.org/badges/version/rgee)](https://cran.r-project.org/package=rgee)

`rgee` is a bindings package for calling [Google Earth Engine
API](https://developers.google.com/earth-engine/) from within R.
Additionally, several functions have been implemented to make the
connection with the R spatial ecosystem effortless. The `rgee` package
structure has been inspired in [tensorflow R
package](https://github.com/rstudio/tensorflow/).

## What is Google Earth Engine?

[Google Earth Engine](https://earthengine.google.com/) is a cloud-based
platform that allows users getting access to a petabyte-scale archive of
remote sensing data and run geospatial analysis on Google’s
infrastructure. Currently, Google offers support Python and JavaScript.
`rgee` will fill the gap and start offering support to R\!. Below we
compare the syntax between `rgee` and the two Google-supported client
libraries.

**Earth Engine Javascript API**:

``` javascript
image = ee.Image('CGIAR/SRTM90_V4')
print(image.bandNames())
#> 'elevation'
```

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

**Quite similar, isn’t it?**. However there are a few additional small
changes that you always need to keep in mind when use Google Earth
Engine with R. Please check the [consideration section]() before start
coding\!

## Requirements

Prior to using `rgee` you need to install a **Python version higher than
3.5** in your system. Below we describe how to install `rgee` but not
Python dependencies. Consider check the [setup section]() for
customizing Python installation.

## Installation

Install the `rgee` package from GitHub is extremely simple, just run in
your R console as follow:

``` r
remotes::install_git("csaybar/rgee")
```

`rgee` depends on [sf](https://github.com/r-spatial/sf). Therefore, it
is necessary to install their external libraries, run as follows
according your operating system:

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

Use [Homebrew](https://brew.sh/) to install the next system libraries:

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

## Package Conventions

  - All `rgee` functions begin with the prefix ee\_. Auto-completion is
    your friend :).
  - Complete access to the Earth Engine API with the prefix
    [**ee$…:**](https://developers.google.com/earth-engine/).
  - Authenticate and Initialize Earth Engine with
    [**ee\_Initialize:**](https://csaybar.github.io/rgee/reference/ee_Initialize.html)
    it is a wrapper around `ee$Initialize` that offers multi-user
    support.
  - `rgee` is “pipe-friendly” and, in fact, re-exports %\>%, but does
    not require its use.
  - Wrap your R function using `ee_pyfunc` before passing them to the
    Earth Engine Web REST API. This is not compulsory, but it will help
    reduce possible :bug:.

## Quick Demo

### Compute the trend of night-time lights

Authenticate and Initialize Earth Engine.

``` r
library(rgee)
ee_Initialize()
ee_reattach() # reattach ee as a reserve word
```

Adds a band containing image date as years since 1991.

``` r
createTimeBand <-function(img) {
  year <- ee$Date(img$get('system:time_start'))$get('year')$subtract(1991L)
  ee$Image(year)$byte()$addBands(img)
}
```

Map the time band creation helper over the [night-time lights
collection](https://developers.google.com/earth-engine/datasets/catalog/NOAA_DMSP-OLS_NIGHTTIME_LIGHTS).

``` r
collection = ee$
  ImageCollection('NOAA/DMSP-OLS/NIGHTTIME_LIGHTS')$
  select('stable_lights')$
  map(createTimeBand)
```

Compute a linear fit over the series of values at each pixel,
visualizing the y-intercept in green, and positive/negative slopes as
red/blue.

``` r
col_reduce <- collection$reduce(ee$Reducer$linearFit())
col_reduce <- col_reduce$addBands(
  col_reduce$select('scale'))
ee_print(col_reduce)
```

Create interactive visualizations\!

``` r
ee_map(eeobject = col_reduce,
       vizparams = list(min=0, max= c(0.18, 20, -0.18)),
       bands=c('scale', 'offset', 'scale'),
       objname = 'stable lights trend')
```

![rgee\_01](https://user-images.githubusercontent.com/16768318/71565699-51e4a500-2aa9-11ea-83c3-9e1d32c82ba6.png)

### Extract precipitation values

Load `sf` and authenticate and initialize Earth Engine.

``` r
library(rgee)
library(sf)
ee_Initialize()
# ee_reattach() # reattach ee as a reserve word
```

Read the `nc` shapefile as a simple
feature.

``` r
nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE) %>%
  st_transform(4326) # Transform coordinates
```

Map each image from 2001 and extract the Monthly precipitation
accumulation (Pr) of the [Terraclimate
dataset](https://developers.google.com/earth-engine/datasets/catalog/IDAHO_EPSCOR_TERRACLIMATE)

``` r
terraclimate <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
  filterDate("2000-01-01", "2001-01-01")$
  map(ee_pyfunc(function(x) x$select("pr")))
```

Extract values from the Terraclimate ImageCollection. `ee_extract` works
similar to `raster::extract` you just need to define: the
ImageCollection object (x), the geometry (y), and a function to
summarize the values
(fun).

``` r
ee_nc_rain <- ee_extract(x = terraclimate, y = nc, fun = ee$Reducer$max(), id = "FIPS")
colnames(ee_nc_rain) <- c("FIPS", month.abb)
head(ee_nc_rain)
```

<table class="table" style="margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

FIPS

</th>

<th style="text-align:right;">

Jan

</th>

<th style="text-align:right;">

Feb

</th>

<th style="text-align:right;">

Mar

</th>

<th style="text-align:right;">

Apr

</th>

<th style="text-align:right;">

May

</th>

<th style="text-align:right;">

Jun

</th>

<th style="text-align:right;">

Jul

</th>

<th style="text-align:right;">

Aug

</th>

<th style="text-align:right;">

Sep

</th>

<th style="text-align:right;">

Oct

</th>

<th style="text-align:right;">

Nov

</th>

<th style="text-align:right;">

Dec

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

37009

</td>

<td style="text-align:right;">

93

</td>

<td style="text-align:right;">

68

</td>

<td style="text-align:right;">

106

</td>

<td style="text-align:right;">

168

</td>

<td style="text-align:right;">

73

</td>

<td style="text-align:right;">

97

</td>

<td style="text-align:right;">

117

</td>

<td style="text-align:right;">

107

</td>

<td style="text-align:right;">

166

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

89

</td>

<td style="text-align:right;">

56

</td>

</tr>

<tr>

<td style="text-align:left;">

37005

</td>

<td style="text-align:right;">

85

</td>

<td style="text-align:right;">

64

</td>

<td style="text-align:right;">

99

</td>

<td style="text-align:right;">

165

</td>

<td style="text-align:right;">

66

</td>

<td style="text-align:right;">

96

</td>

<td style="text-align:right;">

107

</td>

<td style="text-align:right;">

106

</td>

<td style="text-align:right;">

163

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

83

</td>

<td style="text-align:right;">

53

</td>

</tr>

<tr>

<td style="text-align:left;">

37171

</td>

<td style="text-align:right;">

95

</td>

<td style="text-align:right;">

54

</td>

<td style="text-align:right;">

87

</td>

<td style="text-align:right;">

143

</td>

<td style="text-align:right;">

59

</td>

<td style="text-align:right;">

114

</td>

<td style="text-align:right;">

101

</td>

<td style="text-align:right;">

119

</td>

<td style="text-align:right;">

162

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

67

</td>

<td style="text-align:right;">

48

</td>

</tr>

<tr>

<td style="text-align:left;">

37053

</td>

<td style="text-align:right;">

122

</td>

<td style="text-align:right;">

50

</td>

<td style="text-align:right;">

67

</td>

<td style="text-align:right;">

118

</td>

<td style="text-align:right;">

135

</td>

<td style="text-align:right;">

183

</td>

<td style="text-align:right;">

142

</td>

<td style="text-align:right;">

213

</td>

<td style="text-align:right;">

174

</td>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

72

</td>

<td style="text-align:right;">

49

</td>

</tr>

<tr>

<td style="text-align:left;">

37131

</td>

<td style="text-align:right;">

115

</td>

<td style="text-align:right;">

49

</td>

<td style="text-align:right;">

63

</td>

<td style="text-align:right;">

108

</td>

<td style="text-align:right;">

115

</td>

<td style="text-align:right;">

163

</td>

<td style="text-align:right;">

152

</td>

<td style="text-align:right;">

195

</td>

<td style="text-align:right;">

132

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

57

</td>

<td style="text-align:right;">

44

</td>

</tr>

<tr>

<td style="text-align:left;">

37091

</td>

<td style="text-align:right;">

122

</td>

<td style="text-align:right;">

43

</td>

<td style="text-align:right;">

64

</td>

<td style="text-align:right;">

109

</td>

<td style="text-align:right;">

121

</td>

<td style="text-align:right;">

169

</td>

<td style="text-align:right;">

146

</td>

<td style="text-align:right;">

200

</td>

<td style="text-align:right;">

143

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

57

</td>

<td style="text-align:right;">

43

</td>

</tr>

</tbody>

</table>

Create a simple plot\!

``` r
ee_nc_rain <- merge(nc, ee_nc_rain, by = "FIPS")
plot(ee_nc_rain["Jan"], main = "2001 Jan Precipitation - Terraclimate", reset = FALSE)
```

![ee\_JAN](https://user-images.githubusercontent.com/16768318/71566261-1c8e8600-2aae-11ea-9f02-71b16f05c9d0.png)

## How does it works?

`rgee` is **not** a native Earth Engine API like the Javascript or Python client, to do this would be extremely hard, especially considering that the API is in [active development](https://github.com/google/earthengine-api). So, how it is possible to run Earth Engine in R? the answer is
[reticulate](https://rstudio.github.io/reticulate/). `reticulate` is an R package designed to allow seamless interoperability between R and Python. When we create an Earth Engine process in R, firstly, `reticulate` transforms this piece of code to Python. Once converted to Python code, the `Earth Engine Python API` transform the request to a
`JSON`. Finally, the query is received by the Google Earth Engine Platform thanks to a Web REST API. The response of the server follows the same pathway. If you are searching a way to interact with the Earth Engine Asset (EEA), `rgee` offers also functions to batch [upload](https://csaybar.github.io/rgee/reference/ee_upload.html)([download](https://csaybar.github.io/rgee/reference/ee_download_drive.html)) spatial objects. Additionally, you could easily manage the EEA through the
[ee\_manage\_\*](https://csaybar.github.io/rgee/reference/ee_manage-tools.html) interface.

![workflow](https://user-images.githubusercontent.com/16768318/71569603-3341d680-2ac8-11ea-8787-4dd1fbba326f.png)

## Code of Conduct

Please note that the `rgee` project is released with a [Contributor Code
of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you
agree to abide by its terms.

## Contributing Guide

👍🎉 First off, thanks for taking the time to contribute\! 🎉👍 Please
review our [Contributing Guide](CONTRIBUTING.md).

## Share the love ❤️

Think **rgee** is useful? Let others discover it, by telling them in
person, via Twitter or a blog post.

Using **rgee** for a paper you are writing? Consider citing it

``` r
citation("rgee")
#> 
#> To cite Google Earth Engine in publications use:
#> 
#>   Gorelick, N., Hancher, M., Dixon, M., Ilyushchenko, S., Thau, D., & Moore, R. (2017). 
#>   Google Earth Engine: Planetary scale geospatial analysis for everyone. Remote Sensing
#>   of Environment, 202, 18-27.
```
## Credits :bow:

Most of the functionalities of `rgee` were based in the following third-party R/Python packages:

  - **[gee\_asset\_manager - Lukasz Tracewski](https://github.com/tracek/gee_asset_manager)** 
  - **[geeup - Samapriya Roy](https://github.com/samapriya/geeup)**
  - **[geeadd - Samapriya Roy](https://github.com/samapriya/gee_asset_manager_addon)**
  - **[cartoee - Kel Markert](https://github.com/KMarkert/cartoee)**
  - **[geetools - Rodrigo E. Principe](https://github.com/gee-community/gee_tools)**
  - **[landsat-extract-gee - Loïc Dutrieux](https://github.com/loicdtx/landsat-extract-gee)**
  - **[earthEngineGrabR - JesJehle](https://github.com/JesJehle/earthEngineGrabR)**
  - **[sf - Edzer Pebesma](https://github.com/r-spatial/sf)**
  - **[stars - Edzer Pebesma](https://github.com/r-spatial/stars)**
  - **[gdalcubes - Marius Appel](https://github.com/appelmar/gdalcubes)**

#### Readme template obtained from [dbparser](https://github.com/Dainanahan/dbparser)
