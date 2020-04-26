library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

dataset <- ee$ImageCollection("JRC/GSW1_1/MonthlyRecurrence")$first()

monthlyRecurrence <- dataset$select("monthly_recurrence")
monthlyRecurrenceVis <- list(
  min = 0.0,
  max = 100.0,
  palette = c("ffffff", "ffbbbb", "0000ff")
)

Map$setCenter(-51.482, -0.835, 9)
Map$addLayer(monthlyRecurrence, monthlyRecurrenceVis, "Monthly Recurrence")
