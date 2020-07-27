#' rgee Demo #1: Installation and setup
#' @author Cesar Aybar

# 1. Install from GitHub --------------------------------------------------
remotes::install_github("r-spatial/rgee")
# devtools::install_github("r-spatial/rgee")

# 2. Install Python dependencies
library(reticulate)
# Check which versions of Python will be discovered on a system
py_discover_config()

# Configure which version of Python to use (To avoid this use RETICULATE_ENV)
use_python("/usr/bin/python3")
sys <- import("sys")
sys$executable

# Configure which version of Python to use (To avoid this step use RETICULATE_ENV)
use_python("/usr/bin/python3")
sys <- import("sys")
sys$executable

use_python("/home/aybarpc01/.virtualenvs/rgee/bin/python")
py_config()
py_discover_config()
sys <- import("sys")
sys$executable


# 3. Install Python dependencies (Basic)
library(rgee)
ee_install()
ee_Initialize()

# 4. Other options
# ee_install_set_pyenv()

# 5. Helper functions
ee_help(ee$Algorithms$CannyEdgeDetector)
ee_print(ee$Image(0))


# 6. Considerations

# 6.1 Use ee_utils_pyfunc to avoid erros when map over a ee$List
ee_double_fn <- function(x) ee$Number(x)$add(x)
ee_SimpleList <- ee$List$sequence(0, 12)
ee_NewList <- ee_SimpleList$map(ee_double_fn) # error
ee_NewList <- ee_SimpleList$map(ee_utils_pyfunc(ee_double_fn))
ee_NewList$getInfo()

# 6.2 Dates

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
