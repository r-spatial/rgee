library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

SD <- ee$FeatureCollection("TIGER/2018/States")$
  filter(ee$Filter$eq("STUSPS", "SD"))

ND <- ee$FeatureCollection("TIGER/2018/States")$
  filter(ee$Filter$eq("STUSPS", "ND"))

states <- SD$merge(ND)
Map$centerObject(states, 6)
Map$addLayer(states, name = "Dakotas")
# print(states$size()$getInfo())
