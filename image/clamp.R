library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# ee$Image$clamp() example.

# Clamp the values of all bands in an image to lie within the specified range.
# Values below the low value of that range are set to low value, values above
# the high value of that range are set to the high value.

image <- ee$Image("CGIAR/SRTM90_V4")
clamped <- image$clamp(1000, 2000)

Map$setCenter(-121.753, 46.855, 9)
Map$addLayer(image, list(min = 0, max = 4300), "Full stretch") +
Map$addLayer(clamped, list(min = 0, max = 4300), "Clamped")
