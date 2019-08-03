library(reticulate)
ee_check <- function() {
  #oauth_func_path <- system.file("Python/ee_check.py", package = "rgee")
  oauth_func_path <- "inst/Python/ee_check.py"
  ee_source_python(oauth_func_path)
  python_info <- py_discover_config()
  if (!ee_check_py()) {
    stop(
      sprintf("No module named 'ee' found in %s,",python_info$python),
      "try ee_install() or install manually the following packages:\n",
      "- sudo apt-get install libffi-dev libssl-dev\n",
      "- pip install google-api-python-client\n",
      "- pip install pyCrypto\n",
      "- pip install earthengine-api\n")
  } else {
    cat("The Earth Engine Python API is correctly set up in your system!\n")
  }
}

ee_install <- function() {
  # initialize environments
  env_name <- "rgee"
  # if clean_environment is set to true already an existing environment is deleted
  if (clean_environment) virtualenv_remove(env_name)
}




  # test if environment for dependeicies already exists
  env_test <- grepl(conda_env_name, conda_list()$name)

  # install dependencies via an anaconda environment if test is not treu
  if (!sum(env_test) > 0) {
    tryCatch({
      if (Sys.info()[["sysname"]] != "Linux") {
        conda_create(conda_env_name, packages = c("Python = 2.7", "gdal"))

        conda_install(conda_env_name, packages = c("earthengine-api", "shapely"))

      } else {
        conda_create(conda_env_name,
                     packages = c("Python = 2.7", "gdal=2.1.0", "geos=3.5.0"))
        conda_install(conda_env_name,
                      packages = c("earthengine-api", "shapely", 'oauth2client'))
      }},
      error = function(err)
        stop(paste("Installation problem\n", err), call. = F)
    )
  }
  use_condaenv(conda_env_name)

  # test import of all modules.
  tryCatch({
    test_ee <- py_module_available("ee")
    test_gdal <- py_module_available("gdal")

    if (!test_ee) stop("Module ee could not be imported", call. = F)
    if (!test_gdal) stop("Module gdal could not be imported", call. = F)

  }, error = function(err) {
    test_python()
    test_anaconda()
    stop(paste("Installation problem\n", err), call. = F)
  },
  warning = function(w) {
    warning(w)
  })

  # run authentication ---------------------------------------------------------------

  # if no credential are found run authentication
  if (!test_credentials(with_error = F)) {
    run_oauth_all()
  } else {
    # if credential are found but clean_credentials is set to  true, credentials are deleted and recreated during a new authentication
    if (clean_credentials) {
      delete_credentials()
      run_oauth_all()
    }
  }
  cat("\n \nThe required dependencies are installed and all API's are authenticated for further sessions.\nThere should be no need to run ee_grab_install() again.")
}


library(tic)
install.packages("tic")

remotes::install_github("ropenscilabs/tic")
tic::prepare_all_stages()


get_stage("install") %>%
  # install lwgeom with its own library since linking again postgis source install fails sometimes
  add_code_step(install.packages("lwgeom", configure.args="--without-liblwgeom"))
do_pkgdown(document = FALSE)

tic::script()
