---
title: "NEWS"
output:
  html_document:
    toc: true
    toc_float:
      collapsed: false
      smooth_scroll: false
    toc_depth: 2
vignette: >
  %\VignetteIndexEntry{NEWS}
  %\VignetteEncoding{UTF-8}
---
# rgee 0.5.4
- Earth Engine Python API updated to 0.1.224.
- ee is now an internal rgee environment.
- ee_reattach was deleted.
- ee_print now display the ee$Image properties: system:id, system:time_start and system:time_end.
- rgee now asks users if they would like to save EARTHENGINE_PYTHON in the .Renviron.

# rgee 0.5.3
- Fix a bug in ee_check_python_packages.
- \donttest changed by \dontrun in the documentation.
- gdal and v8 system dependencies added to GH actions.
- Fix a bug in the paper (`ee_extract` example).
- Fix a minor bug in `ee_extract`, now the argument ... works.
- `ee_table_to_drive`: changed the initial value of the argument folder from NULL to "rgee_backup".
- Fix a minor bug in `ee_monitoring`.
- Fix a minor bug in `ee_manage_cancel_all_running_task`.
- Fix a minor bug in `ee_manage_cancel_all_running_task`.
- Improvement in the documentation of `ee_install`.
- Changes in vignettes, `Best Practices` vignette added.

# rgee 0.5.2
- DESCRIPTION: single quotes in title and description.
- DESCRIPTION: A more compressible description of what rgee does.
- DESCRIPTION: Added web reference to the Earth Engine API.
- \dontrun changed by \donttest in all our examples.
- Added "#' @return" to several functions.
- Added "#' @family" to all the functions.
- Added 'quiet' argument to all the functions that needed.
- Added new contributors to rgee (Kevin Ushey, Tim Appelhans, JJ Allaire, Yuan Tang).
- New environmental variable for rgee "EARTHENGINE_INIT_MESSAGE". It will be used to display a message to new users.
- Earth Engine Python API updated to 0.1.223.
- Documentation updated for ee_print and ee_manage_*.
- Fix a bug in ee_install_set_pyenv that did not permit to create properly
the .Renviron_backup file.

# rgee 0.5.1
- ee_install_* functions were deprecated and replaced by ee_install. ee_install create an isolated Python virtual environment with all rgee dependencies.
- ee_install_set_pyenv can be used to set the EARTHENGINE_PYTHON variable.

# rgee 0.5.0
- **Initial CRAN submit**
- Several typos fixed.
- rgee paper added.
- GitHub actions for automated testing and build the website.
- Due the changes in latest `reticulate` version (1.1.5), the functions `ee_install_earthengine_upgrade` and `ee_install_python_packages` were deprecated and both will remove in rgee 0.5.3.
- Config/reticulate added to DESCRIPTION file.
- .onLoad IMPORTANT CHANGES: Now `rgee` set EARTHENGINE_PYTHON instead of RETICULATE_PYTHON directly and RETICULATE_PYTHON_ENV is no longer required. This change will permit users to avoid problems with other R packages that use Python in the backend (such as tensorflow or keras).
- `ee_search_display` function added.
- Several typos fixed in all the documentation.
- Minor changes in `ee_as_sf` to support ee$FeatureCollections without elements.
- `data.colec.fbf` eliminated from all the examples.
- `rgee` now pass all `goodpractice` checks.
- `ee_get_img_date` and `ee_get_ic_date` are now `ee_get_date_img` and `ee_get_date_ic`.
- A new group of functions was created at `ee_utils.R`. `ee_pyfunc` is now `ee_utils_pyfunc`.
- Added new examples in `README.R`.


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
- ee_install_create_pyenv now return a Python Path
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
- ee_Initialize now display reticulate Python version.
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
  - **non-R** installation module added
  - **ee_pyfunc:** Wrap an R function in a Python function with the same signature.
  - **ee_remove_\*:** Delete credentials and selenium drivers.
  
# rgee 0.1.0
- Documentation improved.
- Examples for all functions.
- py_help: Generate compressible documentation in an html format.
- Unit testing in both Python and R.
- Functions added:
  - **ee$...:** Complete access to the Earth Engine API from within R.
  - **ee_Initialize:** Multi-user support for Initialize Earth Engine, a wrapper around `ee$Initialize`.
  - **ee_install:** Interface for installing Selenium drivers and Python packages effortlessly.
  - **ee_check:** Interface for dependencies checking.
  - **ee_manage:** Manage EE assets and tasks recursively.
  - **ee_map:** Display EE spatial objects using [mapview](https://r-spatial.github.io/mapview/).
  - **ee_print:** Print EE metadata.
  - **ee_upload:** Upload ESRI shapefiles or GeoTIFF files to Earth Engine.
  - **ee_download:** Download EE spatial objects.
  - **ee_as_sf** & **sf_as_ee:** Convert EE tables (Feature Collection) to sf and vice-versa.
  - **ee_as_thumbnail:** Create a stars object based on an EE thumbnail image.
  - **ee_extract:** Extract values for EE ImageCollections, similarly to [raster::extract].
  - **ee_search:** Search among the Earth Engine Data Catalog.
  - **ee_help:** Display the Earth Engine documentation.
