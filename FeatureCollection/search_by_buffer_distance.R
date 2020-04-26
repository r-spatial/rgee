library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

#' FeatureCollection Join example. Show parks in San
#' Francisco within 2 kilometers of a BART station.

Map$setCenter(-122.45, 37.75, 13)

bart = ee$FeatureCollection('GOOGLE/EE/DEMOS/bart-locations')
parks = ee$FeatureCollection('GOOGLE/EE/DEMOS/sf-parks')
buffered_bart = bart$map(function(f) f$buffer(2000))

join_filter = ee$Filter$withinDistance(2000, '.geo', NULL, '.geo')
close_parks = ee$Join$simple()$apply(parks, bart, join_filter)

Map$addLayer(buffered_bart, list(color = 'b0b0b0'), "BART Stations")+
Map$addLayer(close_parks, list(color = '008000'), "Parks")
