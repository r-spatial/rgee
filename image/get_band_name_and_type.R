library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

roi <- ee$Geometry$Point(c(-99.2182, 46.7824))

collection <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
  filterBounds(roi)$
  filter(ee$Filter$calendarRange(6, 6, "month"))$
  sort("DATE_ACQUIRED")

print(collection$size()$getInfo())

first <- ee$Image(collection$first())
print(first$bandNames()$getInfo())
print(first$bandTypes()$getInfo())
