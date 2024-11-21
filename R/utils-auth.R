#' Check Python & R sync
#' @noRd
ee_check_init <- function() {

  # if EARTHENGINE_PYTHON is defined, then send it to RETICULATE_PYTHON
  earthengine_python <- Sys.getenv("EARTHENGINE_PYTHON", unset = NA)
  if (!is.na(earthengine_python))
    Sys.setenv(RETICULATE_PYTHON = earthengine_python)

  # Check ee_utils.py sanity
  ee_current_version <- system.file("python/ee_utils.py", package = "rgee")
  if (!file.exists(ee_current_version)) {
    stop(
      sprintf(
        "The file %s does not exist in your system. Please re-install rgee: %s",
        ee_current_version,
        "remotes::install_github(\"r-spatial/rgee\")."
      )
    )
  }


  ee_utils <- ee_connect_to_py(path = ee_current_version, n = 5)
  earthengine_version <- ee_utils_py_to_r(ee_utils$ee_getversion())

  # is earthengine-api greater than 0.1.317?
  if (numeric_version(earthengine_version) < numeric_version("0.1.317")) {
    warning(
      "Update your earthengine-api installations to v0.1.317 or greater. ",
      "Earlier versions are not compatible with recent ",
      "changes to the Earth Engine backend."
    )
  }

  list(earthengine_version = earthengine_version, ee_utils = ee_utils)
}


#' Check Python & R sync
#' @noRd
ee_create_user_subfolder <- function(ee_utils, user) {
  # create a user's folder
  ee_path <- ee_utils_py_to_r(ee_utils$ee_path())
  ee_path_user <- sprintf("%s/%s", ee_path, user)
  dir.create(ee_path_user, showWarnings = FALSE)
}

#' Delete credentials in the main folder
#' @noRd
delete_credentials <- function(ee_utils) {
  eepath <- ee_utils_py_to_r(ee_utils$ee_path())
  eepath_files <- list.files(eepath, full.names = TRUE)
  files_to_remove <- eepath_files[!dir.exists(eepath_files)]
  lapply(files_to_remove, unlink)
}


#' Testing 403 error in GD
#' @noRd
test_drive_privileges <-function(user=NULL) {
  ee_check_packages("ee_Initialize(..., drive=TRUE)", "gargle")

  if (is.null(user)) {
    msg <- sprintf("ee_clean_user_credentials()")
  } else {
    msg <- sprintf("ee_clean_user_credentials('%s')", user)
  }

  # this will break in GD API v4 ...
  req <- gargle::request_build(
    path = "drive/v3/files/{fileId}",
    method = "GET",
    params = list(fileId = "soyunparametro", supportsAllDrives = TRUE),
    token = googledrive::drive_token()
  )
  resp <- gargle::request_make(req)
  if (resp$status_code == 403) {
    stop(
      "Your googledrive token does not have permission\n",
      "to view or modify files from Google Drive.\n",
      "Did you cross the check box when Google asked for permissions?\n",
      "See: https://github.com/r-spatial/rgee/issues/175#issuecomment-905611278\n",
      sprintf(
        "Run %s to fix.",
        crayon::bold(
          msg
        )
      )
    )
  }
}

#' Check if the root folder exist
#' @noRd
ee_check_root_folder <- function() {
  # Root folder exist?
  ee_user_assetroot <- ee$data$getAssetRoots()
  assetroot_exist <- length(ee_user_assetroot) == 0

  # if ee_asset_home (list) length is zero
  if (assetroot_exist) {
    ee_message_05()
    ee_createAssetHome()
    ee_user_assetroot <- ee$data$getAssetRoots()
  }
  ee_user_assetroot_id <- ee_user_assetroot[[1]]$id
  ee_user <- ee_remove_project_chr(ee_user_assetroot_id)
  ee_user
}

#' Wrapper to create a EE Assets home
#' @noRd
ee_createAssetHome <- function() {
  x <- readline("Please insert the desired name of your root folder : users/")
  tryCatch(
    expr = ee$data$createAssetHome(paste0("users/", x)),
    error = function(x) {
      message(
        strsplit(x$message,"\n")[[1]][1],
        " If you have problems creating the ROOT folder use the Earth Engine",
        " Code Editor. \n",
        ">>> https://code.earthengine.google.com/\n",
        ">>> https://raw.githubusercontent.com/csaybar/GCS_AUTH_FILE.json/master/asset_folder.png"
      )
      ee_createAssetHome()
    }
  )
}


#' ee_utils if the first call that rgee does to Python, so delay_load (reticulate::import)
#' will affected. This function was created to force n times the connection to Python virtual
#' env, before to display a error message.
#' @noRd
ee_connect_to_py <- function(path, n = 5) {
  ee_utils <- try(ee_source_python(oauth_func_path = path), silent = TRUE)
  # counter added to prevent problems with reticulate
  con_reticulate_counter <- 1
  while (any(class(ee_utils) %in%  "try-error")) {
    ee_utils <- try(ee_source_python(path), silent = TRUE)
    con_reticulate_counter <- con_reticulate_counter + 1
    if (con_reticulate_counter == (n + 1)) {
      python_path <- reticulate::py_discover_config()
      message_con <- c(
        sprintf("The current Python PATH: %s",
                bold(python_path[["python"]])),
        "does not have the Python package \"earthengine-api\" installed. Do you restarted/terminated",
        "your R session after install miniconda or run ee_install()?",
        "If this is not the case, try:",
        "> ee_install_upgrade(): Install the latest Earth Engine Python version.",
        "> reticulate::use_python(): Refresh your R session and manually set the Python environment with all rgee dependencies.",
        "> ee_install(): To create and set a Python environment with all rgee dependencies.",
        "> ee_install_set_pyenv(): To set a specific Python environment."
      )

      stop(paste(message_con,collapse = "\n"))
    }
  }
  return(ee_utils)
}

#' Display required packages error message
#' @noRd
ee_check_packages <- function(fn_name, packages) {
  pkg_exists <- rep(NA, length(packages))
  counter <- 0
  for(package in packages) {
    counter <- counter + 1
    pkg_exists[counter] <- requireNamespace(package, quietly = TRUE)
  }

  if (!all(pkg_exists)) {
    to_install <- packages[!pkg_exists]
    to_install_len <- length(to_install)
    error_msg <- sprintf(
      "%s required the %s: %s. Please install %s first.",
      bold(fn_name),
      if (to_install_len == 1) "package" else "packages",
      paste0(bold(to_install), collapse = ", "),
      if (to_install_len == 1) "it" else "them"
    )
    stop(error_msg)
  }
}

#' Dataset Creator
#' @noRd
ee_Dataset_creator <- function(eeDataset) {
  eedataset_img <- lapply(eeDataset[["Image"]], function(x) ee$Image(x))
  eedataset_ic <- lapply(eeDataset[["ImageCollection"]], function(x) ee$ImageCollection(x))
  eedataset_fc <- lapply(eeDataset[["FeatureCollection"]], function(x) ee$FeatureCollection(x))
  list(image = eedataset_img, ic = eedataset_ic, fc = eedataset_fc)
}

#' Create session info of the last init inside the
#' folder ~/.config/earthengine/
#' @noRd
ee_sessioninfo <- function(email = NULL,
                           user = NULL,
                           drive_cre = NULL,
                           gcs_cre = NULL) {
  oauth_func_path <- system.file("python/ee_utils.py", package = "rgee")
  utils_py <- ee_source_python(oauth_func_path)
  sessioninfo <- sprintf(
    "%s/rgee_sessioninfo.txt",
    ee_utils_py_to_r(utils_py$ee_path())
  )
  if (is.null(email)) {
    email <- NA
  }
  df <- data.frame(
    email = email, user = user, drive_cre = drive_cre, gcs_cre = gcs_cre,
    stringsAsFactors = FALSE
  )
  write.table(df, sessioninfo, row.names = FALSE)
}


#' Function used in ee_user
#'
#' Search if credentials exist and display
#' it as tick and crosses.
#'
#' @noRd
create_table <- function(user, wsc, quiet = FALSE) {
  oauth_func_path <- system.file("python/ee_utils.py", package = "rgee")
  utils_py <- ee_source_python(oauth_func_path)
  ee_path <- ee_utils_py_to_r(utils_py$ee_path())
  user_clean <- gsub(" ", "", user, fixed = TRUE)
  credentials <- list.files(sprintf("%s/%s",ee_path,user_clean))

  #google drive
  if (any(grepl("@gmail.com",credentials))) {
    gmail_symbol <- green(symbol[["tick"]])
    gd_count <- 1
  } else {
    gmail_symbol <- red(symbol[["cross"]])
    gd_count <- 0
  }

  #GCS
  if (any(grepl(".json",credentials))) {
    gcs_symbol <- green(symbol[["tick"]])
    gcs_count <- 1
  } else {
    gcs_symbol <- red(symbol[["cross"]])
    gcs_count <- 0
  }

  #Earth Engine
  if (any(grepl("credentials",credentials))) {
    ee_symbol <- green(symbol[["tick"]])
    ee_count <- 1
  } else {
    ee_symbol <- red(symbol[["cross"]])
    ee_count <- 0
  }

  if (!quiet) {
    cat("\n",
        user,
        wsc,
        ee_symbol,
        wsc,
        gmail_symbol,
        wsc,
        gcs_symbol
    )
  }
  user_str <- data.frame(EE = ee_count, GD = gd_count, GCS = gcs_count)
  row.names(user_str) <- user_clean
  invisible(user_str)
}

#' Function used in ee_user
#'
#' Add extra space to usernames to form a nice table
#'
#' @noRd
add_extra_space <- function(name, space) {
  iter <- length(space)
  result <- rep(NA,iter)
  for (z in seq_len(iter)) {
    add_space <- paste0(rep(" ",space[z]), collapse = "")
    result[z] <- paste0(name[z], add_space)
  }
  result
}

#' Read and evaluate a python script
#' @noRd
ee_source_python <- function(oauth_func_path) {
  module_name <- gsub("\\.py$", "", basename(oauth_func_path))
  module_path <- dirname(oauth_func_path)
  import_from_path(module_name, path = module_path, convert = FALSE)
}


# messages ----------------------------
#' Long message display 01 - rule and user
#' @noRd
ee_message_01 <- function(user=NULL, earthengine_version) {
  cat(
    cli::rule(
      left = crayon::bold("rgee", packageVersion("rgee")),
      right = paste0("earthengine-api ", earthengine_version)
    ), "\n"
  )

  if (is.null(user)) {
    user <- "not_defined"
  }

  cat(
    "", crayon::green(cli::symbol[["tick"]]),
    crayon::blue("user:"),
    crayon::green(user), "\n"
  )
}


#' Long message display 03 - drive
#' @noRd
ee_message_02 <- function(init=TRUE) {
  if (init) {
    cat(
      "",
      crayon::green(cli::symbol$tick),
      crayon::blue("Google Drive credentials:")
    )
  } else {
    cat(
      "\r",
      crayon::green(cli::symbol$tick),
      crayon::blue("Google Drive credentials:"),
      # drive_credentials,
      crayon::green(" FOUND\n")
    )
  }
}



#' Long message display 01 - GCS
#' @noRd
ee_message_03 <- function(init=TRUE, gcs_credentials) {
  if (init) {
    cat(
      "",
      crayon::green(cli::symbol[["tick"]]),
      crayon::blue("GCS credentials:")
    )
  } else {
    if (!is.na(gcs_credentials[["path"]])) {
      cat(
        "\r",
        crayon::green(cli::symbol[["tick"]]),
        crayon::blue("GCS credentials:"),
        # gcs_credentials,
        crayon::green(" FOUND\n")
      )
    } else {
      cat(
        "\r",
        crayon::green(cli::symbol[["tick"]]),
        crayon::blue("GCS credentials:"),
        # gcs_credentials,
        crayon::red("NOT FOUND\n")
      )
    }
  }
}

#' Long message display 01 - GEE
#' @noRd
ee_message_04 <- function(init=TRUE) {
  if (init) {
    cat(
      "", crayon::green(cli::symbol[["tick"]]),
      crayon::blue("Initializing Google Earth Engine:")
    )
  } else {
    cat(
      "\r",
      crayon::green(cli::symbol[["tick"]]),
      crayon::blue("Initializing Google Earth Engine:"),
      crayon::green(" DONE!\n")
    )
  }
}

#' Long message display 01 - GEE ROOT folder
#' @noRd
ee_message_05 <- function() {
  root_text <- paste(
    "Earth Engine Assets home root folder does not exist for the current user.",
    "Please enter your desired root folder name below. Take into consideration",
    sprintf("that once you created %s",
            crayon::bold("you will not be able to change the folder name again. ")),
    sprintf("press ESC to interrupt and run: %s",
            crayon::bold("ee$data$createAssetHome(\"users/PUT_YOUR_NAME_HERE\")")),
    sprintf("to attempt to create it. After that execute again %s.",
            crayon::bold("ee_Initialize()")),
    sep = "\n"
  )
  message(root_text)
}


#' Long message display 01 - GEE ROOT folder
#' @noRd
ee_message_06 <- function(gcs_credentials, ee_user) {
  cat("\r", crayon::green(cli::symbol[["tick"]]), crayon::blue("Earth Engine account:"),
      crayon::green(crayon::bold(ee_user)), "\n")
  py_ee_config <- reticulate::py_config()
  cat("\r", crayon::green(cli::symbol[["tick"]]), crayon::blue("Python Path:"),
      crayon::green(crayon::bold(py_ee_config[["python"]])), "\n")
  cat(cli::rule(), "\n")
  if (!is.na(gcs_credentials[["message"]])) {
    message(gcs_credentials[["message"]])
  }
}


