# Spatial R packages ---------------------------------------------------------------
library(rgee)
library(sf)
library(cptcity)
library(mapview)
library(leaflet)

ee_Initialize() # Initialize Google Earth Engine (Just One time)

# 1. world map ------------------------------------------------------------
cpt_pal <- cpt(pal = "mpl_inferno")
image <- ee$Image('CGIAR/SRTM90_V4')
ee_Map$centerObject(eeObject = image)
ee_Map$addLayer(eeObject = image,
                visParams = list(min = 0, max = 5000, palette= cpt_pal),
                name = 'SRTM90_V4')
# 2. sf map ---------------------------------------------------------------
nc = st_read(system.file("shape/nc.shp", package="sf"))
cpt_pal <- cpt(pal = "wkp_schwarzwald_wiki_schwarzwald_cont")

nc_ee <- nc %>%
  st_transform(4326) %>%
  sf_as_ee(check_ring_dir = TRUE)

clip_image <- image$clip(nc_ee)

mapview(nc, alpha.regions = 0, legend = FALSE) +
  ee_Map$addLayer(clip_image,
                  list(min = 0, max = 1000, palette= cpt_pal),
                  'SRTM')

# 3. Extract values from Earth Engine to sf  ------------------------------
nc_dem  <- ee_extract(clip_image,nc_ee,id = "FIPSNO") %>%
  `names<-`(c("FIPSNO",'SRTM_DEM')) %>%
  merge(nc, .)
plot(nc_dem['SRTM_DEM'])
