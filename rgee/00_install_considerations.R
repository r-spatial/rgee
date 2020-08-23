#' Demo N°01: Good practices in rgee installation
#' https://csaybar.github.io/rgee-examples/
#' @author Cesar Aybar

# -------------------------------
# 1. Install rgee and reticulate
# -------------------------------

remotes::install_github("r-spatial/rgee")
remotes::install_github("rstudio/reticulate")

# Please if you have any problem installing rgee reported in
# https://github.com/r-spatial/rgee/issues/new


# -------------------------------
# 2. Load libraries
# -------------------------------
library(reticulate)
library(rgee)

# Use the py_discover_config() function to see what version of Python will be
# used without actually loading Python (no initialize R-Python connection!)
py_discover_config()

# Use the py_config() function to query for information about the specific 
# version of Python IN USE as well as a list of other Python versions discovered
# on the system. (initialize R-Python connection!)
py_config()

# Verify current Python path 
import("sys")$executable

# -------------------------------
# 3. Set a Python Env
# -------------------------------

# >>> rgee needs that the Python version IN USE is Python3.
# >>> rgee (reticulate) needs that your system is x64 rather than x86.
# >>> If you are a Window users, rgee (reticulate) needs conda environment (see
#     https://docs.conda.io/en/latest/miniconda.html), linux or MacOS users could
#     use either virtualenv or conda.
use_python("PUT_HERE_THE_PYTHON3_VERSION_PATH_TO_BE_USE_TO_CREATE_A_PYTHON_ENV_TO_RGEE")

# -------------------------------
# 4. Set a Python Env to rgee
# -------------------------------
ee_install()

# -------------------------------
# 5. Upgrade rgee!
# -------------------------------
ee_install_upgrade(version = "0.1.232")

# -------------------------------
# 6. Helper functions
# -------------------------------

# https://twitter.com/csaybar/status/1292015511547580416
ee_Initialize()
ee_help(ee$Algorithms$CannyEdgeDetector)

# -------------------------------
# 7. Considerations
# -------------------------------

# 7.1 Use ee_utils_pyfunc to avoid erros when map over a ee$List
ee_double_fn <- function(x) ee$Number(x)$add(x)
ee_SimpleList <- ee$List$sequence(0, 12)
ee_NewList <- ee_SimpleList$map(ee_double_fn) # error
ee_NewList <- ee_SimpleList$map(ee_utils_pyfunc(ee_double_fn))
ee_NewList$getInfo()

# 7.2 Dates

## Load an Image Sentinel2 level-1C
ee_s2 <- ee$ImageCollection("COPERNICUS/S2")$
  filterDate("2016-01-01", "2016-01-31")$
  first()

## From GEE to R
ee_s2$get("system:time_start")$getInfo() # bad
ee_s2$get("system:time_start") %>% eedate_to_rdate() # good!

## From R to GEE
rdate_to_eedate("1981-01-01")
rdate_to_eedate(315532800000) # float number

## Get the date of a ee$Image
ee_get_date_img(ee_s2)

## Get the date of a ee$ImageCollection
library(sf)
nc <- st_read(system.file("shape/nc.shp", package = "sf")) %>%
  st_transform(4326) %>%
  sf_as_ee()

ee_s2 <- ee$ImageCollection("COPERNICUS/S2")$
  filterDate("2016-01-01", "2016-01-31")$
  filterBounds(nc)

ee_get_date_ic(ee_s2)
