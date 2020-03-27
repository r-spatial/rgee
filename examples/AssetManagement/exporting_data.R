library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

image <- ee$Image("USDA/NAIP/DOQQ/m_4609915_sw_14_1_20100629")

# scale means resolution.
downConfig <- list(
  scale = 10,
  maxPixels = 1.0E13,
  driveFolder = "image"
)

task <- ee$batch$Export$image(image, "10m", downConfig)
task$start()

ee_monitoring()
