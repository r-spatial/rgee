library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# NWI legend: https://www.fws.gov/wetlands/Data/Mapper-Wetlands-Legend.html
nwi_add_color <- function(fc) {
  emergent <- ee$FeatureCollection(
    fc$filter(ee$Filter$eq("WETLAND_TY", "Freshwater Emergent Wetland"))
  )
  emergent <- emergent$map(
    function(f) f$set("R", 127)$set("G", 195)$set("B", 28)
  )
  forested <- fc$filter(ee$Filter$eq(
    "WETLAND_TY", "Freshwater Forested/Shrub Wetland"
  ))
  forested <- forested$map(
    function(f) f$set("R", 0)$set("G", 136)$set("B", 55)
  )
  pond <- fc$filter(ee$Filter$eq("WETLAND_TY", "Freshwater Pond"))
  pond <- pond$map(function(f) f$set("R", 104)$set("G", 140)$set("B", 192))

  lake <- fc$filter(ee$Filter$eq("WETLAND_TY", "Lake"))
  lake <- lake$map(function(f) f$set("R", 19)$set("G", 0)$set("B", 124))

  riverine <- fc$filter(ee$Filter$eq("WETLAND_TY", "Riverine"))
  riverine <- riverine$map(
    function(f) f$set("R", 1)$set("G", 144)$set("B", 191)
  )

  fc <- ee$FeatureCollection(emergent$merge(
    forested
  )$merge(pond)$merge(lake)$merge(riverine))

  base <- ee$Image(0)$mask(0)$toInt8()
  base$paint(fc, "R")$addBands(
    base$paint(fc, "G")$addBands(base$paint(fc, "B"))
  )
}
fromFT <- ee$FeatureCollection("users/wqs/Pipestem/Pipestem_HUC10")
Map$centerObject(fromFT)
Map$addLayer(ee$Image()$paint(fromFT, 0, 2), name = "Watershed")

# NWI wetlands for the clicked watershed
nwi_asset_path <- sprintf("users/wqs/NWI-HU8/HU8_%s_Wetlands", "10160002")
clicked_nwi_huc <- ee$FeatureCollection(nwi_asset_path)
nwi_color <- nwi_add_color(clicked_nwi_huc)

Map$centerObject(clicked_nwi_huc, zoom = 10)
Map$addLayer(nwi_color, list(gamma = 0.3, opacity = 0.7), "NWI Wetlands Color")
