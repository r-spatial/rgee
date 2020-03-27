library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

fc <- ee$FeatureCollection('TIGER/2016/Roads')
Map$setCenter(-73.9596, 40.7688, 12)
Map$addLayer(fc, {}, 'Census roads')
