#' print Earth Engine object
#' @param x Earth Engine spatial object.
#' @param ... ignored
#' @param type Character. What to show about the x object?. Three options are
#' supported: "json", "simply", "ee_print". By default "simply".
#' @return No return value, called for displaying Earth Engine objects.
#' @aliases print
#' @export
print.ee.computedobject.ComputedObject <-
  function(x, ..., type = getOption("rgee.print.option")) {
    if (type == "json") {
      str(x)
    } else if (type == "simply") {
      cat(paste0("EarthEngine Object: ", x$name(), "\n"))
    } else if (type == "ee_print") {
      ee_print(x)
    }
  }
