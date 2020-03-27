library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

fc <- ee$FeatureCollection("TIGER/2018/States")

print(fc$first()$getInfo())

new_fc <- fc$select(c("STUSPS", "NAME", "ALAND"), c("abbr", "name", "area"))
print(new_fc$first()$getInfo())

propertyNames <- new_fc$first()$propertyNames()
print(propertyNames$getInfo())
