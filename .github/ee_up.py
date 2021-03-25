import ee
 
words = [
    "#' Earth Engine API version", 
    "#'",
    "#'",
    "#' @family session management functions",
    "#' @return Character. Earth Engine Python API version used to build rgee.",
    "#' @export",
    "ee_version <- function() {",
    " '%s'" % (ee.__version__),
    "}"
]

with open("R/ee_version.R", "w") as file:
    file.write("\n".join(words))