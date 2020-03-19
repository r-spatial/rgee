  <img src="https://raw.githubusercontent.com/csaybar/rgee/master/man/figures/logo.png" align="right" width = 15%/>

# Google Earth Engine for R

**NOTE: Access to Google Earth Engine is only available to [registered users](https://earthengine.google.com/)**.
**The current version of rgee has been built considering the [earthengine-api 0.1.216](https://pypi.org/project/earthengine-api/0.1.216/)**

[![Build
Status](https://travis-ci.org/csaybar/rgee.svg?branch=master)](https://travis-ci.org/csaybar/rgee)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/csaybar/rgee?branch=master&svg=true)](https://ci.appveyor.com/project/csaybar/rgee)
[![codecov](https://codecov.io/gh/csaybar/rgee/branch/master/graph/badge.svg)](https://codecov.io/gh/csaybar/rgee)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![CII Best
Practices](https://bestpractices.coreinfrastructure.org/projects/3527/badge)](https://bestpractices.coreinfrastructure.org/projects/3527)
[![CRAN
status](https://www.r-pkg.org/badges/version/rgee)](https://cran.r-project.org/package=rgee)

### More than 300+ examples using Google Earth Engine with R are available [here](https://csaybar.github.io/rgee-examples/)

`rgee` is a binding package for calling [Google Earth Engine
API](https://developers.google.com/earth-engine/) from within R.
Additionally, several functions have been implemented to make simple the connection with the R spatial ecosystem. The `rgee` package structure has been inspired by the [TensorFlow R
package](https://github.com/rstudio/tensorflow/).

## What is Google Earth Engine?

[Google Earth Engine](https://earthengine.google.com/) is a cloud-based platform that allows users to have an easy access to a petabyte-scale archive of remote sensing data and run geospatial analysis on Google’s infrastructure. Currently, Google offers support only for Python and JavaScript. `rgee` will fill the gap **starting to provide support to R\!**. Below you will find the comparison between the syntax of `rgee` and the two Google-supported client libraries.

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
ee_Initialize()

# Python Style
image <- ee$Image('CGIAR/SRTM90_V4')
image$bandNames()$getInfo()
#> [1] "elevation"

# Or use Pipes instead!!
image <- ee$Image('CGIAR/SRTM90_V4') %>%
  ee$Image$bandNames() %>% 
  ee$List$getInfo()
#> [1] "elevation"
```

**Quite similar, isn’t it?**. However, there are additional smaller changes that you must consider when you use Google Earth Engine with R. Please check the [consideration section](https://csaybar.github.io/rgee/articles/considerations.html) before start coding\!

## Installation

Install the `rgee` package from GitHub is quite simple, you just have to run in your R console as follows:

``` r
remotes::install_github("csaybar/rgee")
```

**`rgee` depends on [sf](https://github.com/r-spatial/sf)**. Therefore, it is necessary to install its external libraries, follow the installation steps specified [here](https://github.com/r-spatial/sf#installing).

#### Docker image
    
    docker pull csaybar/rgee
    docker run -d -p 8787:8787 -e USER=rgee -e PASSWORD=rgee --name rgee-dev csaybar/rgee

After that, in your preferred browser, run:

    127.0.0.1:8787

## Requirements

Prior to using `rgee` you will need to install a **Python version higher than 3.5** in your system. `rgee` counts with a installation module, use it to quickly set up the external dependencies of `rgee`:

```r
library(rgee)

# 1. Initialize rgee with ee_Initialize(). If there is no any Python environment, miniconda
# will be installed by default.
ee_Initialize()

# 2. Create a Python environment, e.g. ee.
ee_create_pyenv(python_env = "ee")

# 3. Find all Python environments  in the system.
ee_discover_pyenvs()

# 4. Set a Python environment (e.g. ee) and restart R to see changes. e.g
ee_set_pyenv(python_path = '/home/user/.virtualenvs/ee/bin/python',
             python_env = 'ee')

# 5. Install Python package dependencies
ee_install_python_packages()

# 6. Initialize rgee again!
ee_Initialize()
```

Additionally, use the functions below, as many times as you want, for checking user info, check sanity of credentials and  Python packages, and remove credentials.

```r
ee_check() # Check non-R dependencies
ee_user_info() # Display credentials information
ee_users() # Display credentials information of all users
ee_remove_credentials() # Remove credentials of a specific user
ee_clean_pyenv() # Remove reticulate system variables
```

Also, consider checking the [setup section](https://csaybar.github.io/rgee/articles/setup.html) for major information to customizing Python installation.


## Package Conventions

  - All `rgee` functions have the prefix ee\_. Auto-completion is
    your friend :).
  - Full access to the Earth Engine API with the prefix
    [**ee$…:**](https://developers.google.com/earth-engine/).
  - Authenticate and Initialize the Earth Engine R API with
    [**ee\_Initialize:**](https://csaybar.github.io/rgee/reference/ee_Initialize.html), you just will need to do it once by session!.
  - `rgee` is “pipe-friendly”, we re-exports %\>%, but `rgee` does
    not require its use.
  - Wrap your R function using `ee_pyfunc` before passing them to the
    Earth Engine Web REST API. This is not compulsory, but it will help
    reduce possible [bugs](https://csaybar.github.io/rgee/articles/considerations.html#the-map-message-error) :bug:.

## Quick Demo

### 1. Compute the trend of night-time lights ([JS version](https://github.com/google/earthengine-api))

Authenticate and Initialize the Earth Engine R API.

``` r
library(rgee)
ee_Initialize()
#ee_reattach() # reattach ee as a reserve word
```

Adds a band containing image date as years since 1991.

``` r
createTimeBand <-function(img) {
  year <- ee$Date(img$get('system:time_start'))$get('year')$subtract(1991L)
  ee$Image(year)$byte()$addBands(img)
}
```

Map the time band creation helper over the [night-time lights collection](https://developers.google.com/earth-engine/datasets/catalog/NOAA_DMSP-OLS_NIGHTTIME_LIGHTS).

``` r
collection = ee$
  ImageCollection('NOAA/DMSP-OLS/NIGHTTIME_LIGHTS')$
  select('stable_lights')$
  map(createTimeBand)
```

Compute a linear fit over the series of values at each pixel, visualizing the y-intercept in green, and positive/negative slopes as red/blue.

``` r
col_reduce <- collection$reduce(ee$Reducer$linearFit())
col_reduce <- col_reduce$addBands(
  col_reduce$select('scale'))
ee_print(col_reduce)
```

Create a interactive visualization\! 

``` r
ee_map(eeobject = col_reduce,
       vizparams = list(min=0, max= c(0.18, 20, -0.18)),
       bands=c('scale', 'offset', 'scale'),
       objname = 'stable lights trend')
```

![rgee\_01](https://user-images.githubusercontent.com/16768318/71565699-51e4a500-2aa9-11ea-83c3-9e1d32c82ba6.png)

### 2. Extract precipitation values

Load `sf` and authenticate and initialize the Earth Engine R API.

``` r
library(rgee)
library(sf)
ee_Initialize()
# ee_reattach() # reattach ee as a reserve word
```

Read the `nc` shapefile.

``` r
nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE) %>%
  st_transform(4326) # Transform coordinates
```

Map each image from 2001 to extract the monthly precipitation (Pr) from the [Terraclimate
dataset](https://developers.google.com/earth-engine/datasets/catalog/IDAHO_EPSCOR_TERRACLIMATE)

``` r
terraclimate <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
  filterDate("2000-01-01", "2001-01-01")$
  map(ee_pyfunc(function(x) x$select("pr")))
```

Extract monthly precipitation values from the Terraclimate ImageCollection through `ee_extract`. `ee_extract` works
similar to `raster::extract` you just need to define: the
ImageCollection object (x), the geometry (y), and a function to
summarize the values (fun).

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

<p align="center">
  <img src="https://user-images.githubusercontent.com/16768318/71566261-1c8e8600-2aae-11ea-9f02-71b16f05c9d0.png">
</p>
  
### 3. Create an NDVI-animation ([JS version](https://developers.google.com/earth-engine/tutorials/community/modis-ndvi-time-series-animation))


Load sf and authenticate and initialize the Earth Engine R API.

``` r
library(rgee)
library(sf)
ee_Initialize()
# ee_reattach() # reattach ee as a reserve word
```

Define the regional bounds of animation frames and a mask to clip the NDVI data by.

``` r
mask <- system.file("shp/arequipa.shp", package = "rgee") %>% 
  st_read(quiet = TRUE) %>% 
  sf_as_ee()
region <- mask$geometry()$bounds()
```

Retrieve the MODIS Terra Vegetation Indices 16-Day Global 1km dataset as an `ee.ImageCollection`
and select the NDVI band.

``` r
col <- ee$ImageCollection('MODIS/006/MOD13A2')$select('NDVI')
```

Group images by composite date

``` r
col <- col$map(function(img) {
  doy <- ee$Date(img$get('system:time_start'))$getRelative('day', 'year')
  img$set('doy', doy)
})
distinctDOY <- col$filterDate('2013-01-01', '2014-01-01')
```

Define a filter that identifies which images from the complete collection match the DOY
from the distinct DOY collection.

``` r
filter <- ee$Filter$equals(leftField = 'doy', rightField = 'doy');
```

Define and Apply the join; convert the resulting FeatureCollection to an ImageCollection.

``` r
join <- ee$Join$saveAll('doy_matches')
joinCol <- ee$ImageCollection(join$apply(distinctDOY, col, filter))
```

Apply median reduction among matching DOY collections.

``` r
comp <- joinCol$map(function(img) {
  doyCol = ee$ImageCollection$fromImages(
    img$get('doy_matches')
  )
  doyCol$reduce(ee$Reducer$median())
})
```

Define RGB visualization parameters.

``` r
visParams = list(
  min = 0.0,
  max = 9000.0,
  bands = "NDVI_median",
  palette = c(
    'FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901',
    '66A000', '529400', '3E8601', '207401', '056201', '004C00', '023B01',
    '012E01', '011D01', '011301'
    )
)
```

Create RGB visualization images for use as animation frames.

```r
rgbVis <- comp$map(function(img) {
  do.call(img$visualize, visParams) %>% 
    ee$Image$clip(mask)
})
```

Define GIF visualization parameters.

```r
gifParams <- list(
  region = region,
  dimensions = 600,
  crs = 'EPSG:3857',
  framesPerSecond = 10
)
```

Render the GIF animation in the console.

```r
print(rgbVis$getVideoThumbURL(gifParams))
browseURL(rgbVis$getVideoThumbURL(gifParams))
```

<p align="center">
  <img src="https://user-images.githubusercontent.com/16768318/77121867-203e0300-6a34-11ea-97ba-6bed74ef4300.gif">
</p>

## How does rgee work?

`rgee` is **not** a native Earth Engine API like the Javascript or Python client, to do this would be extremely hard, especially considering that the API is in [active development](https://github.com/google/earthengine-api). So, how is it possible to run Earth Engine using R? the answer is [reticulate](https://rstudio.github.io/reticulate/). `reticulate` is an R package designed to allow a seamless interoperability between R and Python. When an Earth Engine **request** is created in R, `reticulate` will transform this piece of code to Python. Once the Python code is obtained, the `Earth Engine Python API` transform the request to a `JSON` format. Finally, the query (in JSON) is received by the Google Earth Engine Platform thanks to a Web REST API. The **response** will follow the same path. If you are searching a way to interact with the Earth Engine Asset (EEA), `rgee` offers also functions to batch [upload](https://csaybar.github.io/rgee/reference/sf_as_ee.html)([download](https://csaybar.github.io/rgee/reference/ee_as_sf.html)) spatial objects. Additionally, you could easily manage EEA through the [ee\_manage\_\*](https://csaybar.github.io/rgee/reference/ee_manage-tools.html) interface.

![workflow](https://user-images.githubusercontent.com/16768318/71569603-3341d680-2ac8-11ea-8787-4dd1fbba326f.png)

## Code of Conduct

Please note that the `rgee` project is released with a [Contributor Code
of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.

## Contributing Guide

👍🎉 First off, thanks for taking the time to contribute\! 🎉👍 Please review our [Contributing Guide](CONTRIBUTING.md).

## Share the love ❤️

Think **rgee** is useful? Let others discover it, by telling them in
per on, via Twitter or a blog post.

Using **rgee** for a paper you are writing? Consider citing it

``` r
citation("rgee")
#> 
#> WORKING ON THIS :)
``` 

## Credits :bow:

Most of the `rgee` functionalities were based in the following third-party R/Python packages:

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
