# rgee 0.3.6
- bugs in ee_as_thumbnail and ee_as_stars were fixed.
# rgee 0.3.5
- ee_help display enhanced.
- ee_Initialize() now display reticulate Python version.
- Several bug fixed.
# rgee 0.3.4
- Several bugs fixed
# rgee 0.3.3
- Several bugs fixed
# rgee 0.3.0
- changed ee_map from Map.
- New friendly-user installation module implemented.
- ee_download and ee_upload were deprecated
- selenium functionality deprecated
- ee_as_sf and sf_as_ee now support gcs and drive
- ee_as_stars and stars_as_ee functions created.
- rgee support geometry with different CRS.
- arguments evenOdd, Geodesic, and proj implemented in several functions.
- ee_help permit you display Earth Engine documentation.

# rgee 0.2.0
- Several bugs fixed.
- A collection of 300+ examples for using Google Earth Engine in [R](https://github.com/csaybar/rgee-examples).
- Functions added:
  - [**non-R**](https://github.com/csaybar/rgee#requirements) installation module added
  - [**ee_pyfunc: **](https://csaybar.github.io/rgee/reference/ee_pyfunc.html):Wrap an R function in a Python function with the same signature.
  - [**ee_remove_\*: **](https://csaybar.github.io/rgee/reference/ee_pyfunc.html) Delete credentials and selenium drivers.
  
# rgee 0.1.0
- Documentation improved.
- Examples for all functions.
- py_help: Generate compressible documentation in an html format.
- Unit testing in both Python and R.
- Functions added:
  - [**ee$...:**](https://developers.google.com/earth-engine/) Complete access to the Earth Engine API from within R.
  - [**ee_Initialize:**](https://csaybar.github.io/rgee/reference/ee_Initialize.html) Multi-user support for Initialize Earth Engine, a wrapper around `ee$Initialize`.
  - [**ee_install:**](https://csaybar.github.io/rgee/reference/ee_check-tools.html) Interface for installing Selenium drivers and Python packages effortlessly.
  - [**ee_check:**](https://csaybar.github.io/rgee/reference/ee_check-tools.html) Interface for dependencies checking.
  - [**ee_manage:**](https://csaybar.github.io/rgee/reference/ee_manage-tools.html) Manage EE assets and tasks recursively.
  - [**ee_map:**](https://csaybar.github.io/rgee/reference/ee_map.html) Display EE spatial objects using [mapview](https://r-spatial.github.io/mapview/).
  - [**ee_print:**](https://csaybar.github.io/rgee/reference/ee_print.html) Print EE metadata.
  - [**ee_upload:**](https://csaybar.github.io/rgee/reference/ee_upload.html) Upload ESRI shapefiles or GeoTIFF files to Earth Engine.
  - [**ee_download:**](https://csaybar.github.io/rgee/reference/ee_download.html) Download EE spatial objects.
  - [**ee_as_sf**](https://csaybar.github.io/rgee/reference/ee_upload.html) & [**sf_as_ee:**](https://csaybar.github.io/rgee/reference/ee_download.html) Convert EE tables (Feature Collection) to sf and vice-versa.
  - [**ee_as_thumbnail:**](https://csaybar.github.io/rgee/reference/ee_download.html) Create a stars object based on an EE thumbnail image.
  - [**ee_extract:**](https://csaybar.github.io/rgee/reference/ee_upload.html) Extract values for EE ImageCollections, similarly to [raster::extract](https://www.rdocumentation.org/packages/raster/versions/3.0-2/topics/extract).
  - [**ee_search:**](https://csaybar.github.io/rgee/reference/ee_search.html) Search among the Earth Engine Data Catalog.
  - [**ee_help:**](https://csaybar.github.io/rgee/reference/ee_help.html) Display the Earth Engine documentation.
