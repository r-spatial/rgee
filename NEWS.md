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
# rgee 1.1.1

- Deprecated.R file deleted.
- ee_help Rstudio addin critical bug solved.
- ee_clean argument name changed to 'user' rather than 'email'.
- New test inside ee_Initialize checks if the user token has enough permissions to read or modify files from GD.
- DESCRIPTION file modified: rgee always must use googledrive 2.0.0>=
- rstudioapi package moved from Suggests to Imports.

# rgee 1.1.0

- re-coded the Map and R6Map modules to simplify the maintenance. Many bugs were solved.
- ee_utils_get_crs now call to the web.archive.org if spatialreference is shut down.
- Math module and subsetting module were migrated to [rgeeExtra](https://github.com/r-earthengine/rgeeExtra).
- In ee_Initialize, the "email" parameter was renamed to "user".
- ee_get is now an internal function of rgee. A new/faster version of ee_get is available in rgeeExtra.
- Support to display COG resources. See Map or R6Map examples.
- rgee now supports googledrive version 2.0.0.
- Obtain COG metadata (`ee_utils_cog_metadata`).
- New test unit tests.
- Solve a bug in `ee_help`.
- `Map$addLegend` supports categorical legends in leaflet interactive maps.
- New logos :) 


# rgee 1.0.9

- Accessing the Earth Engine Data Catalog via '$' thanks to the [Earth-Engine-Datasets-List](https://github.com/samapriya/Earth-Engine-Datasets-List) created and supported by [@samapriya](https://github.com/samapriya).
- Math  functions (abs, sign, sqrt, ceiling, cummax, cummin, cumprod, cumsum, 
log, log10, log1p, acos, floor, asin, atan, exp, expm1, cos, cosh, sin, sinh, 
tan, and tanh.) to `ee$Image`.
- Summary functions (max, mean, min, range, sum, product) to `ee$Image`.
- Comparison operators (==, !=, >, <, <=, >=) to `ee$Image`.
- Logic operators (!, &, |) to `ee$Image`.
- Arithmetic operators (+, -, *, /, ^, %%, %/%) to `ee$Image`.
- Subsetting operators ('[[<-', '[[') to `ee$Image` and `ee$ImageCollection`.
- GH Action to automatically updated the Earth Engine Python API.
- `ee_as_sf(..., via = "getInfo")` does not write in temp folder.
- `ee_as_sf` now returns by default a GeoJSON instead of a ESRI shapefile.
- When EarthEngineMaps have the same name, a random hex string is added to the second map.
- Fix a bug in `sf_as_ee` that add `id` column to the results.
- `ee_extract` now supports lazy evaluation and containers `drive` and `gcs`.
- R6, class to display Earth Engine (EE) spatial objects, added.
- Map is now a `R6` object instead of a environment.
- Vignettes documentation upgrade.
- `ee_install_set_pyenv` support local .Renviron. 
- Fix a bug for new tokens in {googledrive} #139.
- Fix a bug in ee_print that sometimes make see the warning: *ee_utils_py_to_r(.) : restarting interrupted promise evaluation*.
- Fix a bug in `ee_install_set_pyenv` when py_env=NULL (#118, thanks @MatthieuStigler).
- GH Action test-coverage removed.
- Fix a bug in `ee_extract` that changes column names when starts with numbers (#119, thanks @joshualerickson).


# rgee 1.0.8

- Unit testing enhanced.
- Fix a bug in Map\$addLayer(..., legend=TRUE) when `eeobject` is an constant image  (i.e. ee\$Image(0)).
- Stop message in `ee_Initialize` to does not allow the use of rgee when the folder ".../rgee/python/" does not exist.
- Info messages when rgee make changes to.Renviron.
- Earth Engine Python API updated to 0.1.247.

# rgee 1.0.7

- Unit testing enhanced.
- More documentation related to credentials.
- Smoother connection with Python (reticulate).
- Now Map$... functions only depend on {leaflet}.
- Public argument added to `ee_as_sf`, `ee_as_raster`, `ee_as_stars`, `ee_imagecollection_to_local`, `ee_drive_to_local` and `ee_gcs_to_local` which permit to create a public link to the resources generated.
- A new argument "**metadata**" is added to `ee_as_sf`, `ee_as_raster`, `ee_as_stars`, `ee_drive_to_local`, `ee_imagecollection_to_local`, and `ee_gcs_to_local`. If TRUE, the metadata related to the export of the images will be added to raster/stars objects.
- Fix a bug in Rstudio `ee_help` addins.
- Fix a bug in `ee_extract` which adds the `system:index` to the colnames when the `x` argument is an `ee$ImageCollection`. 
- Fix a bug that does not permit to `ee_as_raster` and `ee_as_stars` change the fileNamePrefix (#112).
- a stop added in `sf_as_ee` since {geojsonio} does not support POSIXt objects (#113).
- Lazy evaluation support to `ee_imagecollection_to_local`, `ee_as_sf`, `ee_as_raster` and `ee_as_stars`.
- Export images via 'getInfo' was removed from `ee_as_raster` and `ee_as_stars` to avoid problems related to geometric offset.
- Now `ee_monitoring` can also be invoked with the ID of a EE task started.
- `ee_search` module deprecated, it will be removed of rgee in version 1.0.8.
- New functions: `ee_utils_search_display` that display the website related to the Earth Engine dataset, and `ee_utils_future_value` that helps to run a {future} container.
- Earth Engine Python API test updated to 0.1.246.

# rgee 1.0.6
- Class method chaining (i.e. `x$size()$getInfo()`) were changed by pipes (i.e. ee_x %>% `ee$FeatureCollection$size() %>% ee$Number()`) in all the `rgee` functions. This solve the problem "OverflowError: python int too large to convert to C long" on Window systems.
- rgee functions has a cleaner method to run system processes, {**processx**} 
instead of **base::system**. 
- `rgee` I/O functions now check argument before to start to upload/download data.
- Map operators (**+** and **|**) now support EarthEnginemap objects with the 
same name.
- Now `Map$addLayers` only display the legend of the first image.
- Fix a bug in `rgee:::ee_image_local` which makes do not work when all bands have not the same crs and crsTransform.
- "getInfo" method in download raster functions was deprecated and will be removed in v.1.0.8.
- Fix a bug in `sf_as_ee` and `ee_as_sf` now both support SR-ORG CRS codes.
- `ee_users` returns a data.frame.
- `ee_monitoring` counts the processing time.
- Fix a bug in `ee_utils_gif_creator` which makes don't work in windows.
- Several changes in `ee_extract`, now is faster and code is cleaner.
- Fix a bug in name creator in `ee_imagecollection_to_local`.
- A new message more detailed when the Python path does not have the earth-engine Python API.
- Earth Engine Python API updated to 0.1.235.

# rgee 1.0.5
- Important changes in the low level API to upload raster and vector with GCS. However, high upload API (`sf_as_ee`, `stars_as_ee`, and `raster_as_ee`) continue working in the same way.
- Add the functions: `ee_utils_create_manifest_image` and `ee_utils_create_manifest_table`
to help users to create a JSON file with all the upload parameters ("manifest", see https://developers.google.com/earth-engine/image_manifest/).
- Add the functions: `ee_utils_gif_creator`, `ee_utils_gif_annotate` and
`ee_utils_gif_save` to help users to read, add text, and save gif files.
- New | operator inspired in mapview 2.9.0! try: m4 | m5
- Fix several typos.
- Earth Engine Python API updated to 0.1.232.

# rgee 1.0.4
- Add `ee_help` a new Rstudio addins that mimics the help Rstudio interface (F1).
- Fix a bug that makes that `ee_as_sf` only supports `GeoJSON` format.
- If `dsn` is not specified in `ee_as_sf`, it will create a temporary shapefile (in \tmp dir).
- Fix a bug in `ee_imagecollection_to_local` (#87 Thanks @cedlfc44)
- Fix a bug in `ee_image_local` (#88 Thanks @cedlfc44)
- Fix a bug in `ee_create_credentials_drive` (#90 #78 Thanks @cedlfc44)

# rgee 1.0.3
- getPass library removed from `ee_Initialize`.
- New argument `display` in `ee_Initialize` to return the authentication URI. Useful for `rgee` colab users.
- Changes in some diagnostic messages to make possible to use `rgee` in colab.
- `ee_help` returns a HTML file rather than TRUE. It also now supports characters (e.g. `ee_help("ee$Image")`).
- Fix a strange bug when `ee_Initialize` tries to connect to reticulate the first time.
- Fix small bugs in `ee_user_info` and `ee_users`

# rgee 1.0.2
- Earth Engine Python API updated to 0.1.229.
- Fix a bug in `ee_Initialize`, that does not permit users to use `ee_createAssetHome` to define their *Earth Engine Assets home root folder*

# rgee 1.0.1
- Earth Engine Python API updated to 0.1.228.
- `ee_Initialize` now set the global env "RETICULATE_PYTHON" rather than .onLoad

# rgee 1.0.0
- We implement `ee_createAssetHome` to help users to define their *Earth Engine Assets home root folder* without leaving ee_Initialize(). (#70 Thanks @jhollist)
- Fix a bug in `ee_Initialize(drive = TRUE, gcs = TRUE)` which do not permit users save credentials. (#72 Thanks @appelmar).
- Removed `check_ring_dir` argument from `sf_as_ee`. Now `rgee` expect that users fix potential geometry problems before upload geometries to their Earth Engine assets.
- Changes in "welcome to rgee"  message (located in `ee_Initialize`). We delete `stop("Initialization aborted")` and implement `ee_search_init_message`. It will permit to `rgee` to know if the user accepted to create the global var "EARTHENGINE_INIT_MESSAGE" without need to restart the R session.
- Fix minor bugs in `sf_as_ee` and `gcs_to_ee_table`. The argument `command_line_tool_path` was added to give users the option to set the path of the Earth Engine command linetool. This new argument is only relevant to upload files using Google Cloud Storage. New diagnostic message were added.
- Now `sf_as_ee` returns an `ee$Geometry$...` when `x` is a `sfg` or a single sfc object. If `x` is a sf or a `sfc` object with multiple geometries will return an `ee$FeatureCollection`. New unit test for `sf_as_ee`.
- Changes in the documentation of `ee_as_stars` and `ee_as_raster` (#72 Thanks @appelmar).
- Fix minor bugs in `raster_as_ee`, `stars_as_ee` and `gcs_to_ee_image`. The argument `command_line_tool_path` was added to give users the option to set the path of the Earth Engine command linetool. New diagnostic message were added. New unit test added.
- Fixed a bug in `stars_as_ee` that didn't allow to read single-band images.
- `ee_manage_asset_access` has a better approach to determine the user owner of the asset.
- Add a new logical argument called 'strict' to `ee_manage_assetlist`, `ee_manage_copy`,
`ee_manage_move`, `ee_manage_set_properties` and `ee_manage_delete_properties`. If TRUE, 
the existence of the asset will be evaluate before to perform the task. By default TRUE.
- If the `path_asset` is not specified in `ee_manage_assetlist`, rgee will assume that the
path_asset is `ee_get_assethome`.
- `raster_as_ee.R` was created in the /R folder to maintain an order between functions
to upload and download images.
- Fix a bug in the documentation of `print.ee.computedobject.ComputedObject()` (#72 Thanks @appelmar).
- Fix a bug in `ee_install_set_pyenv` now users can set `py_path` without set 
`py_env` and  vice versa.
- `ee_extract` was adapted to work well with changes in `sf_as_ee`.
- R CMD check is more friendly with users, the use of `--run-dontrun` is also 
available (#72 Thanks @appelmar).
- Fix a bug in `ee_get_date_ic`.
- `Map$addLayer` now could display a legend when `eeObject` is an one-band `ee$Image` (By default legend = TRUE).
- New function `ee_get`: Return the element at the specified position in a Earth Engine Collection.
- New function `Map$addLayers`: Create interactive visualizations of ImageCollections.
- Fix a bug: "OverflowError: Python int too large to convert to C long" in `ee_monitoring` (#79 Thanks @andreatitolo).
- Earth Engine Python API updated to 0.1.227.

# rgee 0.6.2
- Earth Engine Python API updated to 0.1.226.
- Fix some typos.
- Fix a minor bug in ee_monitoring.
- Users can mix mapview and EarthEnginemap objects in the same pipeline (see Examples in `Map$addLayer`).
- Add `ee_as_mapview`, a function to convert `EarthEnginemap` objects to  `mapview` objects.
- add a new logical argument called 'strict' to **ee_manage_delete**. If TRUE, the existence of the asset will be evaluate before to perform the task.
- Fix a bug in ee_Initialize, now users without an Earth Engine Assets home root will see a message.
- Fix a minor bug when ee_Initialize change of user, now before to change of user the GCS and GD credentials will be deleted.
- ee_check completely renovated. 
  - New diagnostic messages.
  - Fix a minor bug when testing GCS credentials.
  - The file ee_check.py was deleted.
- Roy Samapriya added as a contributor.  

# rgee 0.6.1
- Fix some typos.
- rgee website update.
- Add citation package option .
- Additional export arguments add to ee_as_stars,ee_as_raster,  ee_imagecollection_to_local and ee_as_sf.

# rgee 0.6.0 
- Earth Engine Python API updated to 0.1.225.
- Fix some typos.
- DESCRIPTION: Moving leaflet, mapview, geojsonio, sf and stars from Import to Suggest. Now users with installation problems can equally use the Earth Engine API although with less operability.
- The 'EarthEngineMap' S4 class was created to avoid incompatibilities with mapview.
- Fix a critical bug in **ee_install** due to the lack of breaks in repeat bucles.
- New function **ee_install_upgrade**.
- New global environment EARTHENGINE_PYTHON was created to help **ee_install_upgrade** to identify the Python environment used by rgee.

# rgee 0.5.4
- Earth Engine Python API updated to 0.1.224.
- Fix a Map typo.
- Fix a bug in ee_as_thumbnail, now the vizparams are checked before to pass to ee$Image$getThumbURL(...).
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
- **First attempt to submit to CRAN**
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
