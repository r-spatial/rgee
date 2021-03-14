#' Earth Engine arithmetic, logic and compare generic functions
#'
#' Arithmetic, logic and compare operators for computation with \code{ee$Image}
#' objects and numeric values.
#'
#' \itemize{
#'   \item \strong{Arith}: +, -, *, /, ^, %%, %/%, %>>% and %>>%.
#'   \item \strong{Logic}: !, &, |.
#'   \item \strong{Comparison}: ==, !=, >, <, <=, >=
#' }
#'
#' @param e1 Numeric or ee$Image.
#' @param e2 Numeric or ee$Image.
#'
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_Initialize()
#'
#' # Sum Operator
#' ee1 <- ee$Image(1)
#' ee2 <- ee$Image(2)
#' ee3 <- ee1 + ee2
#' ee_extract(ee3, ee$Geometry$Point(0, 0))
#'
#' v1 <- 1
#' v2 <- 2
#' v3 <- v1 + v2
#' v3
#'
#' # Multiple Operators
#' ee4 <- ee1 / 10
#' ee5 <- ee4 * (ee2 - 1 + ee1^2 / ee2)
#' ee_extract(ee5, ee$Geometry$Point(0, 0))
#'
#' v4 <- v1 / 10
#' v5 <- v4 * (v2 - 1 + v1^2 / v2)
#' v5
#'
#' # multi-layer object mutiplication, no recycling
#' ee6 <- ee1 + c(1, 5, 10)
#' ee_extract(ee6, ee$Geometry$Point(0, 0))
#'
#' v6 <- v1 + c(1, 5, 10)
#' v6
#' }
#' @name Ops-methods
#' @export
Ops.ee.image.Image <- function(e1, e2) {
  if (.Generic == "+") {
    if (missing(e2)) {
      e1
    } else {
      ee$Image(e1)$add(ee$Image(e2))
    }
  } else if(.Generic == "-") {
    if (missing(e2)) {
      e1$multiply(-1L)
    } else {
      ee$Image(e1)$subtract(ee$Image(e2))
    }
  } else if(.Generic == "*") {
    ee$Image(e1)$multiply(ee$Image(e2))
  } else if(.Generic == "^") {
    ee$Image(e1)$pow(ee$Image(e2))
  } else if(.Generic == "%%") {
    ee$Image(e1)$mod(ee$Image(e2))
  } else if(.Generic == "%/%") {
    ee$Image(e1)$divide(ee$Image(e2))$toInt64()
  } else if(.Generic == "/") {
    ee$Image(e1)$divide(ee$Image(e2))
  } else if (.Generic == "!") {
    if (missing(e2)) {
      ee$Image(e1)$Not()
    } else {
      stop("Unexpected use of !")
    }
  } else if(.Generic == "&") {
    ee$Image(e1)$And(ee$Image(e2))
  } else if(.Generic == "|") {
    ee$Image(e1)$Or(ee$Image(e2))
  } else if(.Generic == "==") {
    ee$Image(e1)$eq(ee$Image(e2))
  } else if(.Generic == "!=") {
    ee$Image(e1)$neq(ee$Image(e2))
  } else if(.Generic == "<") {
    ee$Image(e1)$lt(ee$Image(e2))
  } else if(.Generic == "<=") {
    ee$Image(e1)$lte(ee$Image(e2))
  } else if(.Generic == ">") {
    ee$Image(e1)$gt(ee$Image(e2))
  }  else if(.Generic == ">=") {
    ee$Image(e1)$gte(ee$Image(e2))
  }
}


#' Mathematical functions
#'
#' @param x ee$Image
#' @param ... Ignored
#'
#' Generic mathematical functions that can be used with an \code{ee$Image}
#' object as argument: \code{abs}, \code{sign}, \code{sqrt}, \code{ceiling},
#' \code{cummax}, \code{cummin}, \code{cumprod}, \code{cumsum},
#' \code{log}, \code{log10}, \code{log1p}, \code{acos}, \code{floor},
#' \code{asin}, \code{atan}, \code{exp}, \code{expm1}, \code{cos},
#' \code{cosh}, \code{sin}, \code{sinh}, \code{tan}, and \code{tanh}.
#'
#' @name Math-methods
#' @export
Math.ee.image.Image <- function(x, ...) {
  if (.Generic == "abs") {
    ee$Image$abs(x)
  } else if(.Generic == "signum") {
    ee$Image$signum(x$float())
  } else if(.Generic == "sqrt") {
    ee$Image$sqrt(x)
  } else if(.Generic == "floor") {
    ee$Image$floor(x)
  } else if(.Generic == "ceiling") {
    ee$Image$ceil(x)
  } else if(.Generic == "round") {
    ee$Image$round(x)
  } else if(.Generic == "log") {
    args <- list(...)
    if (length(args) == 0) {
      ee$Image$log(x) / ee$Image$log(ee$Image$exp(1))
    } else {
      ee$Image$log(x) / ee$Image$log(...)
    }
  } else if(.Generic == "log10") {
    ee$Image$log10(x)
  } else if(.Generic == "log1p") {
    ee$Image$log(x + 1)
  } else if(.Generic == "exp") {
    ee$Image$exp(x)
  } else if(.Generic == "expm1") {
    ee$Image$exp(x) - 1
  } else if(.Generic == "sin") {
    ee$Image$sin(x)
  } else if(.Generic == "cos") {
    ee$Image$cos(x)
  } else if(.Generic == "tan") {
    ee$Image$tan(x)
  } else if(.Generic == "asin") {
    ee$Image$asin(x)
  } else if(.Generic == "acos") {
    ee$Image$acos(x)
  } else if(.Generic == "atan") {
    ee$Image$atan(x)
  } else if(.Generic == "cosh") {
    ee$Image$cosh(x)
  } else if(.Generic == "sinh") {
    ee$Image$sinh(x)
  } else if(.Generic == "tanh") {
    ee$Image$tanh(x)
  } else if(.Generic == "cumsum") {
    total <- 0
    x_list <- list() # List to save.
    x_bandnames <- x$bandNames()$getInfo() #band names.
    for (index in seq_along(x_bandnames)) {
      total <- total + x[[x_bandnames[[index]]]]
      x_list[[index]] <- total
    }
    ee$ImageCollection(x_list)$toBands()
  } else if(.Generic == "cumprod") {
    total <- 1
    x_list <- list() # List to save.
    x_bandnames <- x$bandNames()$getInfo() #band names.
    for (index in seq_along(x_bandnames)) {
      total <- total * x[[x_bandnames[[index]]]]
      x_list[[index]] <- total
    }
    ee$ImageCollection(x_list)$toBands()
  } else {
    stop("rgee does not support yet ", .Generic)
  }
}

#' Summary Methods
#'
#' @param ... ee$Image.
#' @param na.rm Ignore.
#' @name Summary-methods
#' @export
Summary.ee.image.Image <- function(..., na.rm = TRUE) {
  imgs <- list(...)

  if (length(imgs) > 1) {
    stop("Only one ee$Image is supported.")
  }
  img <- imgs[[1]]

  if (.Generic == "max") {
    img$reduce(ee$Reducer$max())
  } else if (.Generic == "min") {
    img$reduce(ee$Reducer$max())
  } else if (.Generic == "range") {
    img$reduce(ee$Reducer$minMax())
  } else if (.Generic == "sum") {
    img$reduce(ee$Reducer$sum())
  } else if (.Generic == "prod") {
    img$reduce(ee$Reducer$product())
  } else {
    stop("rgee does not support yet ", .Generic)
  }
}

#' @name Summary-methods
#' @export
mean.ee.image.Image <- function(..., na.rm = TRUE) {
  imgs <- list(...)
  if (length(imgs) > 1) {
    stop("Only one ee$Image is supported.")
  }
  img <- imgs[[1]]
  img$reduce(ee$Reducer$mean())
}

