library(rgee)
ee_Initialize()

# ee_help
# Print documentation for Earth Engine objects
ee$Image %>% ee_help()
ee$ImageCollection$toList %>% ee_help()

# ee_print:
# Print and return metadata about Spatial Earth Engine Objects
l8 <- ee$Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318")
!ee_print(l8)

ic_l8 <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")
ee_print(ic_l8)

# Use aside for debugging
ee_ok <- function(x) {
  is_list <- x$getInfo()
  if (class(is_list) == "list") {
    message("OK!")
  }
}

# Use pipe (%>%)rather than sign ($)
l8 %>%
  ee$Image$aside(ee_ok) %>%
  ee$Image$select("B1") %>%
  ee$Image$aside(ee_ok) %>%
  ee$Image$projection() %>%
  ee$Image$aside(ee_ok)
