<a href="https://colab.research.google.com/github/r-spatial/rgee/blob/examples/rgee_colab.ipynb"><img align="left" src="https://colab.research.google.com/assets/colab-badge.svg" alt="Open in Colab" title="Open and Execute in Google Colaboratory"></a>
[![R build status](https://github.com/r-spatial/rgee/workflows/R-CMD-check/badge.svg)](https://github.com/r-spatial/rgee/actions)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![codecov](https://codecov.io/gh/r-spatial/rgee/branch/master/graph/badge.svg)](https://codecov.io/gh/r-spatial/rgee)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![status](https://joss.theoj.org/papers/aea42ddddd79df480a858bc1e51857fc/status.svg)](https://joss.theoj.org/papers/aea42ddddd79df480a858bc1e51857fc)
[![CRAN
status](https://www.r-pkg.org/badges/version/rgee)](https://cran.r-project.org/package=rgee)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3945409.svg)](https://doi.org/10.5281/zenodo.3945409)

# Google Earth Engine for R

<a href="http://r-spatial.github.io/rgee"><img src="https://raw.githubusercontent.com/r-spatial/rgee/master/man/figures/logo.png" align="left" hspace="10" vspace="0" width="15%"></a>

`rgee` is a binding package for calling [Google Earth Engine
API](https://developers.google.com/earth-engine/) from within R.
Additionally, several functions have been implemented to make simple the connection with the R spatial ecosystem. The current version of rgee has been built considering the 
[earthengine-api 0.1.231](https://pypi.org/project/earthengine-api/0.1.231/).
**Note that access to Google Earth Engine is only available to [registered users](https://earthengine.google.com/)**.


### More than 250+ examples using GEE with R are available [here](http://csaybar.github.io/rgee-examples/)

<a href="https://github.com/r-spatial/rgee/blob/examples/FeatureCollection/search_by_buffer_distance.R"><img src="https://raw.githubusercontent.com/ryali93/rgee_readme_icons/master/images/img_01_search_by_buffer_distance.png" height="100"/></a>
<a href="https://github.com/r-spatial/rgee/blob/examples/image/convolutions.R"><img src="https://raw.githubusercontent.com/ryali93/rgee_readme_icons/master/images/img_02_convolutions.png" height="100"/></a>
<a href="https://github.com/r-spatial/rgee/blob/examples/FeatureCollection/idw_interpolation.R"><img src="https://raw.githubusercontent.com/ryali93/rgee_readme_icons/master/images/img_03_idw_interpolation.png" height="100"/></a>
<a href="https://github.com/r-spatial/rgee/blob/examples/image/spectral_unmixing.R"><img src="https://raw.githubusercontent.com/ryali93/rgee_readme_icons/master/images/img_04_spectral_unmixing.png" height="100"/></a>
<a href="https://github.com/r-spatial/rgee/blob/examples/Algorithms/CloudMasking/sentinel-2.R"><img src="https://raw.githubusercontent.com/ryali93/rgee_readme_icons/master/images/img_05_sentinel2.png" height="100"/></a>
<a href="https://github.com/r-spatial/rgee/blob/examples/image/canny_edge_detector.R"><img src="https://raw.githubusercontent.com/ryali93/rgee_readme_icons/master/images/img_06_canny_edge_detector.png" height="100"/></a>
<a href="https://github.com/r-spatial/rgee/blob/examples/image/cumulative_cost_mapping.R"><img src="https://raw.githubusercontent.com/ryali93/rgee_readme_icons/master/images/img_07_cumulative_cost_mapping.png" height="100"/></a>
<a href="https://github.com/r-spatial/rgee/blob/examples/image/zero_crossing.R"><img src="https://raw.githubusercontent.com/ryali93/rgee_readme_icons/master/images/img_08_zero_crossing.png" height="100"/></a>
<a href="https://github.com/r-spatial/rgee/blob/examples//Visualization/hillshade.R"><img src="https://raw.githubusercontent.com/ryali93/rgee_readme_icons/master/images/img_09_hillshade.png" height="100" hspace="5"/></a>
<a href="https://github.com/r-spatial/rgee/blob/examples/Visualization/styled_layer_descriptors.R"><img src="https://raw.githubusercontent.com/ryali93/rgee_readme_icons/master/images/img_10_styled_layer_descriptors.png" height="100"/></a>
<a href="https://github.com/r-spatial/rgee/blob/examples/Visualization/terrain_visualization.R"><img src="https://raw.githubusercontent.com/ryali93/rgee_readme_icons/master/images/img_11_terrain_visualization.png" height="100" hspace="5"/></a>
<a href="https://github.com/r-spatial/rgee/blob/examples/Datasets/Vectors/us_census_counties.R"><img src="https://raw.githubusercontent.com/ryali93/rgee_readme_icons/master/images/img_12_us_census_counties.png" height="100"/></a>
<a href="https://github.com/r-spatial/rgee/blob/examples/Datasets/Vectors/global_power_plant_database.R"><img src="https://raw.githubusercontent.com/ryali93/rgee_readme_icons/master/images/img_13_global_power_plant_database.png" height="100"/></a>
<a href="https://github.com/r-spatial/rgee/blob/examples/Datasets/Vectors/landsat_wrs2_grid.R"><img src="https://raw.githubusercontent.com/ryali93/rgee_readme_icons/master/images/img_14_landsat_wr2_grid.png" height="100" hspace="4"/></a>
<a href="https://github.com/r-spatial/rgee/blob/examples/Datasets/Water/jrc_metadata.R"><img src="https://raw.githubusercontent.com/ryali93/rgee_readme_icons/master/images/img_15_jrc_metadata.png" height="100"/></a>
<a href="https://github.com/r-spatial/rgee/blob/examples//Visualization/visualizing_geometries.R"><img src="https://raw.githubusercontent.com/ryali93/rgee_readme_icons/master/images/img_16_visualizing_geometries.png" height="100" hspace="1"/></a>
<a href="https://github.com/r-spatial/rgee/blob/examples/Algorithms/center_pivot_irrigation_detector.R"><img src="https://raw.githubusercontent.com/ryali93/rgee_readme_icons/master/images/img_17_center_pivot_irrigation_detector.png" height="100" hspace="2"/></a>
<a href="https://github.com/r-spatial/rgee/blob/examples/Visualization/image_color_ramp.R"><img src="https://raw.githubusercontent.com/ryali93/rgee_readme_icons/master/images/img_18_image_color_ramp.png" height="100"/></a>

### Quick Start User's Guide for rgee 
<a href="http://amazeone.com.br/barebra/pandora/rgeebookT1eng.pdf"><img src="https://user-images.githubusercontent.com/16768318/88080933-5b1c8880-cb45-11ea-9546-f0640da13997.png" height="99"/></a>
<a href="http://amazeone.com.br/barebra/pandora/rgeebookT1.pdf"><img src="https://user-images.githubusercontent.com/16768318/86457619-8ef45300-bce9-11ea-9f08-7c1ee14071fb.png" height="100"/></a>
<a href="https://barja8.github.io/Handbook_rgee/pdf/vol01.pdf"><img src="https://user-images.githubusercontent.com/16768318/86457622-90be1680-bce9-11ea-92f0-78cfb915c4bc.png" height="101"/></a>

**Created by:**
- EN and POR: Andres Luiz Lima Costa <http://amazeone.com.br/index.php>
- SPA: Antony Barja Ingaruca <https://barja8.github.io/>
### 


## What is Google Earth Engine?

[Google Earth Engine](https://earthengine.google.com/) is a cloud-based platform that allows users to have an easy access to a petabyte-scale archive of remote sensing data and run geospatial analysis on Google’s infrastructure. Currently, Google offers support only for Python and JavaScript. `rgee` will fill the gap **starting to provide support to R\!**. Below you will find the comparison between the syntax of `rgee` and the two Google-supported client libraries.

<table>
<tr>
<th> JS (Code Editor) </th>
<th> Python </th>
<th> R </th>
</tr>
<tr>
<td>
  
``` javascript
var db = 'CGIAR/SRTM90_V4'
var image = ee.Image(db)
print(image.bandNames())
#> 'elevation'
```

</td>
<td>

``` python
import ee
ee.Initialize()
db = 'CGIAR/SRTM90_V4'
image = ee.Image(db)
image.bandNames().getInfo()
#> [u'elevation']
```

</td>
<td>

``` r
library(rgee)
ee_Initialize()
db <- 'CGIAR/SRTM90_V4'
image <- ee$Image(db)
image$bandNames()$getInfo()
#> [1] "elevation"
```
</td>
</tr>
</table>

**Quite similar, isn’t it?**. However, there are additional smaller changes should consider when using Google Earth Engine with R. Please check the [consideration section](https://r-spatial.github.io/rgee/articles/considerations.html) before you start coding\!

## Installation

Install the `rgee` package from GitHub is quite simple, you just have to run in your R console as follows:

``` r
remotes::install_github("r-spatial/rgee")
```

**`rgee` depends on [sf](https://github.com/r-spatial/sf)**. Therefore, is necessary to install its external libraries, follow the installation steps specified [here](https://github.com/r-spatial/sf#installing). If you are using a Debian-based operating system, you probably need to install **virtualenv** as well.

```
sudo pip3 install virtualenv
```

#### Docker image
    
    docker pull csaybar/rgee
    docker run -d -p 8787:8787 -e USER=rgee -e PASSWORD=rgee --name rgee-dev csaybar/rgee

After that, in your preferred browser, run:

    127.0.0.1:8787

## setup

Prior to using `rgee` you will need to install a **Python version higher than 3.5** in their system. `rgee` counts with an installation function (ee_install) which helps to setup `rgee` correctly:

```r
library(rgee)

## It is necessary just once
ee_install()

# Initialize Earth Engine!
ee_Initialize()
```

Additionally, you might use the functions below for checking the status of rgee dependencies and delete credentials.

```r
ee_check() # Check non-R dependencies
ee_clean_credentials() # Remove credentials of a specific user
ee_clean_pyenv() # Remove reticulate system variables
```

Also, consider looking at the [setup section](https://r-spatial.github.io/rgee/articles/setup.html) for more information on customizing your Python installation.

## Package Conventions

  - All `rgee` functions have the prefix ee\_. Auto-completion is
    your friend :).
  - Full access to the Earth Engine API with the prefix
    [**ee$…**](https://developers.google.com/earth-engine/).
  - Authenticate and Initialize the Earth Engine R API with
    [**ee\_Initialize**](https://r-spatial.github.io/rgee/reference/ee_Initialize.html). It is necessary once by session!.
  - `rgee` is “pipe-friendly”, we re-exports %\>%, but `rgee` does not require its use.

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
collection <- ee$
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
Map$setCenter(9.08203, 47.39835, 3)
Map$addLayer(
  eeObject = col_reduce,
  visParams = list(
    bands = c("scale", "offset", "scale"),
    min = 0,
    max = c(0.18, 20, -0.18)
  ),
  name = "stable lights trend"
)

```

![rgee\_01](https://user-images.githubusercontent.com/16768318/71565699-51e4a500-2aa9-11ea-83c3-9e1d32c82ba6.png)

### 2. Extract precipitation values

Install and load `tidyverse` and `sf` R package, after that, initialize the Earth Engine R API. 

``` r
library(tidyverse)
library(rgee)
library(sf)

# ee_reattach() # reattach ee as a reserve word
ee_Initialize()
```

Read the `nc` shapefile.

``` r
nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
```

Map each image from 2001 to extract the monthly precipitation (Pr) from the [Terraclimate
dataset](https://developers.google.com/earth-engine/datasets/catalog/IDAHO_EPSCOR_TERRACLIMATE)

``` r
terraclimate <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE")$
  filterDate("2001-01-01", "2002-01-01")$
  map(function(x) x$reproject("EPSG:4326")$select("pr"))
```

Extract monthly precipitation values from the Terraclimate ImageCollection through `ee_extract`. `ee_extract` works
similar to `raster::extract`, you just need to define: the ImageCollection object (x), the geometry (y), and a function to
summarize the values (fun).

``` r
ee_nc_rain <- ee_extract(x = terraclimate, y = nc, sf = FALSE)
colnames(ee_nc_rain) <- sprintf("%02d", 1:12)
ee_nc_rain$name <- nc$NAME
```

Use ggplot2 to generate a beautiful static plot!

``` r
ee_nc_rain %>%
  pivot_longer(-name, names_to = "month", values_to = "pr") %>%
  ggplot(aes(x = month, y = pr, group = name, color = pr)) +
  geom_line(alpha = 0.4) +
  xlab("Month") +
  ylab("Precipitation (mm)") +
  theme_minimal()
```

<p align="center">
  <img src="https://user-images.githubusercontent.com/16768318/81945044-2cbd8280-95c3-11ea-9ef5-fd9f6fd5fe89.png" width=80%>
</p>


### 3. Create an NDVI-animation ([JS version](https://developers.google.com/earth-engine/tutorials/community/modis-ndvi-time-series-animation))

Install and load `sf`, after that, initialize the Earth Engine R API.

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

Define a join; convert the resulting FeatureCollection to an ImageCollection.

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

`rgee` is **not** a native Earth Engine API like the Javascript or Python client, to do this would be extremely hard, especially considering that the API is in [active development](https://github.com/google/earthengine-api). So, how is it possible to run Earth Engine using R? the answer is [reticulate](https://rstudio.github.io/reticulate/). `reticulate` is an R package designed to allow a seamless interoperability between R and Python. When an Earth Engine **request** is created in R, `reticulate` will transform this piece into Python. Once the Python code is obtained, the `Earth Engine Python API` transform the request to a `JSON` format. Finally, the request is received by the Google Earth Engine Platform thanks to a Web REST API. The **response** will follow the same path. 

![workflow](https://user-images.githubusercontent.com/16768318/71569603-3341d680-2ac8-11ea-8787-4dd1fbba326f.png)

## Code of Conduct

Please note that the `rgee` project is released with a [Contributor Code
of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.

## Contributing Guide

👍 Thanks for taking the time to contribute\! 🎉👍 Please review our [Contributing Guide](CONTRIBUTING.md).

## Share the love  ❤️
Think **rgee** is useful? Let others discover it, by telling them in
person via Twitter or a blog post.

Using **rgee** for a paper you are writing? Consider citing it

``` r
citation("rgee")
To cite rgee in publications use:

  C Aybar, Q Wu, L Bautista, R Yali and A Barja (2020) rgee: An R
  package for interacting with Google Earth Engine Journal of Open
  Source Software URL https://github.com/r-spatial/rgee/.

A BibTeX entry for LaTeX users is

  @Article{,
    title = {rgee: An R package for interacting with Google Earth Engine},
    author = {Cesar Aybar and Quisheng Wu and Lesly Bautista and Roy Yali and Antony Barja},
    journal = {Journal of Open Source Software},
    year = {2020},
  }
``` 

## Credits :bow:

First off, we would like to offer an **special thanks** :raised_hands: :clap: to [**Justin Braaten**](https://github.com/jdbcode) for his wise and helpful comments in the whole development of **rgee**. As well, we would like to mention the following third-party R/Python packages for contributing indirectly to the develop of rgee:

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
