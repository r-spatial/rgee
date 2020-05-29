.onAttach <- function(libname, pkgname) {
  # Message for new user
  init_rgee_message <- Sys.getenv("EARTHENGINE_INIT_MESSAGE", unset = NA)
  if (is.na(init_rgee_message)) {
    text <- paste(
      crayon::bold("Welcome to the Earth Engine client library for R!"),
      "It seems it is your first time using rgee. Before start coding is ",
      sprintf(
        "necessary to set up a Python environment. Run %s",
        crayon::bold("rgee::ee_install()")
      ),
      "to set up automatically, after that, restart the R session to see",
      "changes. rgee wraps a Python session into an R session. Therefore,",
      "like the Earth Engine Python API, you will need to initialize",
      sprintf(
        "the API before starting coding. Run %s",
        crayon::bold("rgee::ee_Initialize()")
      ),
      "to accomplish this task. See more than 250+ examples of rgee at",
      crayon::bold("https://csaybar.github.io/rgee-examples/"),
      "",
      sep = "\n"
    )
    packageStartupMessage(text)
    ee_install_set_init_message()
  }
  options(rgee.print.option = "simply")
}

.onLoad <- function(libname, pkgname) {
  # Configure a Python environment
  reticulate::configure_environment(pkgname)

  # if EARTHENGINE_PYTHON is defined then forward it to RETICULATE_PYTHON
  earthengine_python <- Sys.getenv("EARTHENGINE_PYTHON", unset = NA)
  if (!is.na(earthengine_python))
    Sys.setenv(RETICULATE_PYTHON = earthengine_python)

  ee <<- reticulate::import("ee", delay_load = TRUE)
}


#' Reattach ee as a reserved word
#' @family session management functions
#' @export
ee_reattach <- function() {
  ee <<- reticulate::import("ee", delay_load = TRUE)
}
