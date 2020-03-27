library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

image <- ee$Image('USDA/NAIP/DOQQ/m_4609915_sw_14_1_20100629')
Map$centerObject(image)
Map$addLayer(image, list(bands = c('N', 'R', 'G')), 'NAIP')
