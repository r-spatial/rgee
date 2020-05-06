# rgee 0.4.3
- ee_extract improved: when RGEE_NAME is not defined, columns name will use the band name for `ee$Images` and the `system:index` property for `ee$ImageCollections`. Various bugs fixed.
- ee_manage upgrade. documentation enhanced.
- I/O module upgrade.
# rgee 0.4.2
- ee_image_to_local and ee_local_image_as_ee removed.
- ee_image_as_stars and ee_image_as_raster now is ee_as_stars and ee_as_raster.
- ee_gcs_to_image renamed to gcs_to_ee_image.
- minor bug in ee_print solved.
# rgee 0.4.1
- ee_extract bug fixed now sf arg works for Earth Engine tables.
- ee_create_pyenv now return a Python Path
- doc upgrade
# rgee 0.4
- travis and appveyor replaced by github-actions.
- docker folder deleted and saved in docker branch.
- examples folder deleted and saved in examples branch.
- Changes in installation module.
- Readme and documentation upgrade.
- new test added.
# rgee 0.3.16
- New test added.
- minor bugs solved
# rgee 0.3.15
- git action button added.
- minor bugs solved
# rgee 0.3.14
- ee_image module upgrade: new tests, new doc, and several bugs fixed.
- functions ee_gcs_to_asset_* renamed to ee_gcs_to_*
# rgee 0.3.13
- new function inside the ee_search interface: `ee_datacatalog_display`
- sf_as_ee doc upgrade.
- `ee_Image_to_local`: now region arg must be a ee$Geometry$Rectangle only when via = "getInfo" is used. It is will affect also to download of `ee$ImageCollections`.
# rgee 0.3.12
- Minor changes in ee_Initialize and ee_install-
- ee_version passed from 0.1.127 to 0.1.128.
# rgee 0.3.11
- sf_as_ee now permits their use not just in polygons.
- ee_as_thumbnail bug: it does not work in one-band images (fixed).
- ee_user_info new display.
- ee_Initialize now does not display Python version used (use ee_user_info instead).
# rgee 0.3.10
- ee_imagecollection_to_local local added.
- ee_as_stars changed by ee_as_stars.
- ee_image_stars and ee_image_to_local added
- ee_extract new argument added "sf = FALSE".
- ee_as_thumbnail arguments modified.
- ee_print new display.
- ee_help bug fixed.
- ee_fix_offset function added to support image downloads by getInfo.
# rgee 0.3.9
- ee_date functions upgraded
- Documentation enhanced
- Several bug fixed
# rgee 0.3.8
- ee_as_sf support the download of large tables
- Several bug fixed
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
