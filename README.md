
<img src="https://raw.githubusercontent.com/csaybar/pal/master/man/figures/logo.png" align="right" width = 10%/>

# rgee

[![Travis build
status](https://travis-ci.org/ryali93/rgee.svg?branch=master)](https://travis-ci.org/ryali93/rgee)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/ryali93/rgee?branch=master&svg=true)](https://ci.appveyor.com/project/ryali93/rgee)
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

### Buildings for the Python Earth Engine API

**rgee** provides a convenient way to interact with the Google Earth
Engine since R. It does not interact directly with the Earth Engine REST
APIs but, through `reticulate`, create a stable bridge with the Python
Earth Engine API.

The main user relevant functions are:

  - `ee_initialize`: Authenticate and Initialize the Earth Engine API.
  - `ee_map`: Display a given ee\_Image, ee\_Feature,
    ee\_FeatureCollection or ee\_ImageCollection using
    [`mapview`](https://github.com/r-spatial/mapview).
  - `ee_download`: Download Earth Engine objects (via
    `googledrive::drive_download()`)

## Installation (Not available yet)

For CRAN release version of **rgee** use

``` r
install.packages("rgee")
```

To install the development version install the
[devtools](https://cran.r-project.org/package=devtools)
package.

``` r
devtools::install_github("ryali93/rgee")
```

## Usage

![workflow](https://raw.githubusercontent.com/ryali93/rgee/master/man/figures/rgee.png)

## Contact

Please file bug reports and feature requests at
<https://github.com/ryali93/rgee/issues>

In case of Pull Requests, please make sure to submit them to the develop
branch of this repository.
