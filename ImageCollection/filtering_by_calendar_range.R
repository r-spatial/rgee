library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

roi <- ee$Geometry$Point(c(-99.2182, 46.7824))

# find images acquired during June and July
collection <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
  filterBounds(roi)$
  filter(ee$Filter$calendarRange(6, 7, "month"))$
  sort("DATE_ACQUIRED")

print(collection$size()$getInfo())

first <- collection$first()
propertyNames <- first$propertyNames()
print(propertyNames$getInfo())

time_start <- ee$Date(first$get("system:time_start"))$format("YYYY-MM-dd")
print(time_start$getInfo())
