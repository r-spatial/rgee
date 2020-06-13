#' print Earth Engine object
#' @param x Earth Engine spatial object.
#' @param type Character. What to show about the x object?. Three options are
#' supported: "json", "simply", "ee_print". By default "simply".
#' @param ... ignored
print.ee.computedobject.ComputedObject <-
  function(x, type = getOption("rgee.print.option"), ...) {
    if (type == "json") {
      str(x)
    } else if (type == "simply") {
      cat(paste0("EarthEngine Object: ", x$name()))
    } else if (type == "ee_print") {
      ee_print(x)
    }
  }


#' Method for printing EarthEngineMap objects
#' @param x a EarthEngineMap object
setMethod('print', signature(x = "EarthEngineMap"),
          function(x) {
            print(methods::slot(x, "map"))
          }
)

#' Method for printing EarthEngineMap objects (show)
#' @param object a EarthEngineMap object
setMethod("show", signature(object = "EarthEngineMap"),
          function(object)
          {
            print(methods::slot(object, "map"))
          }
)
