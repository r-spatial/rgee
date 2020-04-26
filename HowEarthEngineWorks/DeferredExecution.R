library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()


image = ee$Image('CGIAR/SRTM90_V4')
operation = image$add(10)

print(operation$getInfo()) # open container!
print(operation) # API request
