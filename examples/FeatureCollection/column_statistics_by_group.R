library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# Load a collection of US census blocks.
blocks <- ee$FeatureCollection("TIGER/2010/Blocks")

# Compute sums of the specified properties, grouped by state code.
sums <- blocks$filter(
  ee$Filter$And(
    ee$Filter$neq("pop10", list()),
    ee$Filter$neq("housing10", list())
  )
) %>%
  ee$FeatureCollection$reduceColumns(
    selectors = list("pop10", "housing10", "statefp10"),
    reducer = ee$Reducer$sum()$`repeat`(2)$group(
      groupField = 2,
      groupName = "state-code"
    )
  )

# Print the resultant Dictionary.
print(sums$getInfo())
