library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

intersect <- function(state) {
  nPowerPlants <- ee$List(state$get("power_plants"))$size()
  # Return the state feature with a new property: power plant count.
  state$set("n_power_plants", nPowerPlants)
}


# Load the primary 'collection': US state boundaries.
states <- ee$FeatureCollection("TIGER/2018/States")

# Load the secondary 'collection': power plants.
powerPlants <- ee$FeatureCollection("WRI/GPPD/power_plants")

# Define a spatial filter as geometries that intersect.
spatialFilter <- ee$Filter$intersects(
  leftField = ".geo",
  rightField = ".geo",
  maxError = 10
)

# Define a save all join.
saveAllJoin <- ee$Join$saveAll(
  matchesKey = "power_plants"
)

# Apply the join.
intersectJoined <- saveAllJoin$apply(states, powerPlants, spatialFilter)

# Add power plant count per state as a property.
intersectJoined <- intersectJoined$map(intersect)
print(intersectJoined$getInfo())
