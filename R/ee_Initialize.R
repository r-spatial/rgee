#' Authenticate and Initialize Earth Engine
#'
#' Authorize rgee to manage Earth Engine resources, Google
#' Drive, and Google Cloud Storage. The \code{ee_initialize()} via
#' web-browser will ask users to sign into your Google account and
#' allows you to grant permission to manage resources. This function is
#' a wrapper around `rgee::ee$Initialize()`.
#'
#' @param user Character (optional, e.g. `data.colec.fbf`). The user
#' argument is used to create a folder inside the path
#' \code{~/.config/earthengine/} that save all the credentials for a specific
#' Google identity.
#'
#' @param drive Logical (optional). If TRUE, the drive credential
#' is cached in the path \code{~/.config/earthengine/}.
#'
#' @param gcs Logical (optional). If TRUE, the Google Cloud Storage
#' credential is cached in the path \code{~/.config/earthengine/}.
#'
#' @param display Logical. If TRUE, display the earthengine authentication URL.
#'
#' @param quiet Logical. Suppress info messages.
#'
#' @importFrom utils read.table browseURL write.table packageVersion
#' @importFrom reticulate import_from_path import install_miniconda py_available
#' @importFrom cli symbol rule
#' @importFrom crayon blue green black red bold white
#'
#' @details
#' \code{ee_Initialize(...)} can manage Google Drive, and Google
#' Cloud Storage resources using the R packages googledrive and
#' googlecloudStorageR, respectively. By default, rgee does not require
#' them. These are only necessary to enable rgee I/O functionality.
#' All user credentials are saved in the directory
#' \code{~/.config/earthengine/}. If a user does not specify the "user"
#' argument, all user credentials are saved in the the subdirectory
#' \code{~/.config/earthengine/ndef}.
#'
#' @family session management functions
#' @return No return value, called for initializing the earthengine-api.
#' @examples
#' \dontrun{
#' library(rgee)
#'
#' # Simple init - Load just the Earth Engine credential
#' ee_Initialize()
#' ee_user_info()
#' }
#' @export
ee_Initialize <- function(user = NULL,
                          drive = FALSE,
                          gcs = FALSE,
                          display = FALSE,
                          quiet = FALSE) {
  # Message for new user
  init_rgee_message <- ee_search_init_message()
  if (!init_rgee_message) {
    ee_init_message()
  }

  # Load your Python Session
  # if EARTHENGINE_PYTHON is defined, then send it to RETICULATE_PYTHON
  earthengine_python <- Sys.getenv("EARTHENGINE_PYTHON", unset = NA)
  if (!is.na(earthengine_python))
    Sys.setenv(RETICULATE_PYTHON = earthengine_python)

  # get the path of earth engine credentials
  ee_current_version <- system.file("python/ee_utils.py", package = "rgee")
  if (!file.exists(ee_current_version)) {
    stop(
      sprintf(
        "The file %s does not exist in their system. Please re-install rgee: %s",
        ee_current_version,
        "remotes::install_github(\"r-spatial/rgee\")."
      )
    )
  }
  ee_utils <- ee_connect_to_py(path = ee_current_version, n = 5)
  earthengine_version <- ee_utils_py_to_r(ee_utils$ee_getversion())

  if (!quiet) {
    cat(
      rule(
        left = bold("rgee", packageVersion("rgee")),
        right = paste0("earthengine-api ", earthengine_version)
      ), "\n"
    )
  }

  # is earthengine-api greater than 0.1.215?
  if (as.numeric(gsub("\\.","",earthengine_version)) < 01215) {
    warning(
      "Update your earthnengine-api installations to v0.1.232 or greater. ",
      "Earlier versions are not compatible with recent ",
      "changes to the Earth Engine backend."
    )
  }

  # 1. simple checking
  if (is.null(user)) {
    user <- "ndef"
  }

  if (!quiet) {
    if (user == "ndef") {
      cat(
        "", green(symbol[["tick"]]),
        blue("user:"),
        green("not_defined\n")
      )
    } else {
      cat(
        "", green(symbol[["tick"]]),
        blue("user:"),
        green(user), "\n"
      )
    }
  }

  # create a user's folder
  email_clean <- gsub("@gmail.com", "", user)
  ee_path <- ee_utils_py_to_r(ee_utils$ee_path())
  ee_path_user <- sprintf("%s/%s", ee_path, email_clean)
  dir.create(ee_path_user, showWarnings = FALSE, recursive = TRUE)

  ## remove previous gd and gcs credentials
  unlink(list.files(ee_path, "@gmail.com", full.names = TRUE))
  unlink(list.files(ee_path, ".json", full.names = TRUE))

  # Loading all the credentials: earthengine, drive and GCS.
  drive_credentials <- NA
  gcs_credentials <- list(path = NA, message = NA)

  if (drive) {
    ee_check_packages("ee_Initialize", "googledrive")
    # your user have enough permissions to read GD files?
    if (!quiet) {
      cat(
        "",
        green(symbol$tick),
        blue("Google Drive credentials:")
      )
    }
    drive_credentials <- ee_create_credentials_drive(user, quiet = quiet)
    test_drive_privileges(user)
    if (!quiet) {
      cat(
        "\r",
        green(symbol$tick),
        blue("Google Drive credentials:"),
        # drive_credentials,
        green(" FOUND\n")
      )
    }
  }

  if (gcs) {
    ee_check_packages("ee_Initialize", "googleCloudStorageR")
    if (!quiet) {
      cat(
        "",
        green(symbol[["tick"]]),
        blue("GCS credentials:")
      )
    }
    gcs_credentials <- tryCatch(
      expr = ee_create_credentials_gcs(user),
      error = function(e) {
        list(path = NA, message = NA)
      })

    if (!quiet) {
      if (!is.na(gcs_credentials[["path"]])) {
        cat(
          "\r",
          green(symbol[["tick"]]),
          blue("GCS credentials:"),
          # gcs_credentials,
          green(" FOUND\n")
        )
      } else {
        cat(
          "\r",
          green(symbol[["tick"]]),
          blue("GCS credentials:"),
          # gcs_credentials,
          red("NOT FOUND\n")
        )
      }
    }
  }
  ## rgee session file
  options(rgee.gcs.auth = gcs_credentials[["path"]])
  if (!quiet) {
    cat(
      "", green(symbol[["tick"]]),
      blue("Initializing Google Earth Engine:")
    )
  }

  ee_create_credentials_earthengine(email_clean, display = display)
  ee$Initialize()

  if (!quiet) {
    cat(
      "\r",
      green(symbol[["tick"]]),
      blue("Initializing Google Earth Engine:"),
      green(" DONE!\n")
    )
  }

  # Root folder exist?
  ee_user_assetroot <- ee$data$getAssetRoots()
  assetroot_exist <- length(ee_user_assetroot) == 0
  # if ee_asset_home (list) length is zero
  if (assetroot_exist) {
    root_text <- paste(
      "Earth Engine Assets home root folder does not exist for the current user.",
      "Please enter your desired root folder name below. Take into consideration",
      sprintf("that once created %s Alternatively,",
              bold("you will not be able to change the folder name again. ")),
      sprintf("press ESC to interrupt and run: %s",
              bold("ee$data$createAssetHome(\"users/PUT_YOUR_NAME_HERE\")")),
      sprintf("to attempt to create it. After that execute again %s.",
              bold("ee_Initialize()")),
      sep = "\n"
    )
    message(root_text)
    ee_createAssetHome()
    ee_user_assetroot <- ee$data$getAssetRoots()
  }
  ee_user_assetroot_id <- ee_user_assetroot[[1]]$id
  ee_user <- ee_remove_project_chr(ee_user_assetroot_id)

  options(rgee.ee_user = ee_user)
  ee_sessioninfo(
    email = email_clean,
    user = ee_user,
    drive_cre = drive_credentials,
    gcs_cre = gcs_credentials[["path"]]
  )

  if (!quiet) {
    cat("\r", green(symbol[["tick"]]), blue("Earth Engine account:"),
        green(bold(ee_user)), "\n")
    cat(rule(), "\n")
    if (!is.na(gcs_credentials[["message"]])) {
     message(gcs_credentials[["message"]])
    }
  }
  # ee_check_python_packages(quiet = TRUE)

  # Add Dataset attribute
  eeDataset <- jsonlite::read_json(system.file("dataset.json", package="rgee"))
  eeDataset_b <- ee_Dataset_creator(eeDataset)

  ee$FeatureCollection$Dataset <- eeDataset_b$fc
  ee$ImageCollection$Dataset <- eeDataset_b$ic
  ee$Image$Dataset <- eeDataset_b$image

  invisible(TRUE)
}

#' Authorize rgee to view and manage your Earth Engine account.
#' This is a three-step function:
#' \itemize {
#' \item First get the full path name of the Earth Engine credentials
#' considering the email address.
#' \item Second, use the file.copy function to set up the
#' "credentials" file, so that the Earth Engine Python API can read it.
#' \item Finally, if the file.copy fails at copy it, the credentials
#' will download from Internet. You will be directed to a web browser.
#' Sign in to your Google account to be granted rgee
#' permission to operate on your behalf with Google Earth Engine.
#' These user credentials are cached in a folder below your
#' home directory, `rgee::ee_get_earthengine_path()`, from
#' where they can be automatically refreshed, as necessary.
#' }
#' @noRd
ee_create_credentials_earthengine <- function(email_clean, display) {
  oauth_func_path <- system.file("python/ee_utils.py", package = "rgee")
  utils_py <- ee_source_python(oauth_func_path)

  # first step
  ee_path <- ee_utils_py_to_r(utils_py$ee_path())
  main_ee_credential <- sprintf("%s/credentials", ee_path)
  user_ee_credential <- sprintf(
    "%s/%s/credentials",
    ee_path,
    email_clean
  )
  # second step
  path_condition <- file.exists(user_ee_credential)
  if (isTRUE(path_condition)) {
    path_condition <- file.copy(
      from = user_ee_credential,
      to = main_ee_credential,
      overwrite = TRUE
    )
  } else {
    oauth_codes <- ee_utils_py_to_r(utils_py$create_codes())
    code_verifier <- oauth_codes[[1]]
    code_challenge <- oauth_codes[[2]]
    earthengine_auth <- ee$oauth$get_authorization_url(code_challenge)
    # Display URL?
    if (display) {
      message("\n To authorize access needed by Earth Engine, open the following URL in a web browser and follow the instructions: \n \n", earthengine_auth, "\n \n The authorization workflow will generate a code, which you should paste in the box below")
      auth_code <- readline("Enter Earth Engine Authentication: ")
      token <- ee$oauth$request_token(auth_code, code_verifier)
      credential <- sprintf('{"refresh_token":"%s"}', token)
      write(credential, main_ee_credential)
      write(credential, user_ee_credential)
    }
    ee_save_eecredentials(
      url = earthengine_auth,
      code_verifier = code_verifier,
      main_ee_credential = main_ee_credential,
      user_ee_credential = user_ee_credential
    )
    invisible(TRUE)
  }
}

#' Save EE credentials
#' @noRd
ee_save_eecredentials <- function(url, code_verifier, main_ee_credential, user_ee_credential) {
  browseURL(url)
  auth_code <- readline("Enter Earth Engine Authentication: ")
  token <- ee$oauth$request_token(auth_code, code_verifier)
  credential <- sprintf('{"refresh_token":"%s"}', token)
  write(credential, main_ee_credential)
  write(credential, user_ee_credential)
}

#' Create credentials - Google Drive
#' @noRd
ee_create_credentials_drive <- function(email, quiet) {
  ee_check_packages("ee_Initialize", "googledrive")
  # Set folder to save Google Drive Credentials
  oauth_func_path <- system.file("python/ee_utils.py", package = "rgee")
  utils_py <- ee_source_python(oauth_func_path)
  ee_path <- ee_utils_py_to_r(utils_py$ee_path())
  email_clean <- gsub("@gmail.com", "", email)
  ee_path_user <- sprintf("%s/%s", ee_path, email_clean)

  # Load GD credentials (googledrive::drive_auth)
  repeat {
    full_credentials <- list.files(path = ee_path_user, full.names = TRUE)
    drive_condition <- grepl(".*_.*@.*", basename(full_credentials))
    if (!any(drive_condition)) {
      suppressMessages(
        googledrive::drive_auth(
          email = NULL,
          cache = ee_path_user
        )
      )
    } else {
      drive_credentials <- full_credentials[drive_condition]
      email <- sub("^[^_]*_", "", basename(drive_credentials))
      suppressMessages(
        googledrive::drive_auth(
          email = email,
          cache = ee_path_user
        )
      )
      new_full_credentials <- list.files(path = ee_path_user, full.names = TRUE)
      new_drive_condition <- grepl(".*_.*@.*", basename(new_full_credentials))
      if (sum(new_drive_condition) > 1) {
        files_credentials_time <- file.info(new_full_credentials[new_drive_condition])$ctime
        drive_credential_to_remove <- new_full_credentials[which.min(files_credentials_time)]
        if (!quiet) {
          message("Removing previous Google Drive Token ....")
        }
        file.remove(drive_credential_to_remove)
      }
      break
    }
  }

  # Clean previous and copy new GD credentials in ./earthengine folder
  clean_drive <- list.files(ee_path, ".*_.*@.*", full.names = TRUE) %in% list.dirs(ee_path)
  unlink(
    list.files(ee_path, ".*_.*@.*", full.names = TRUE)[!clean_drive]
  )
  file.copy(
    from = drive_credentials,
    to = sprintf("%s/%s", ee_path, basename(drive_credentials)),
    overwrite = TRUE
  )
  invisible(drive_credentials)
}


#' Authorize Google Cloud Storage
#'
#' Authorize Google Cloud Storage to view and manage your gcs files.
#'
#' @details
#' *.json is the authentication file you have downloaded
#' from your Google Project
#' (https://console.cloud.google.com/apis/credentials/serviceaccountkey/).
#' Is necessary to save it (manually) inside the folder ~/.R/earthengine/USER/.
#' @noRd
ee_create_credentials_gcs <- function(email) {
  # check packages
  ee_check_packages("ee_Initialize", "googleCloudStorageR")

  #get ee path
  oauth_func_path <- system.file("python/ee_utils.py", package = "rgee")
  utils_py <- ee_source_python(oauth_func_path)
  ee_path <- ee_utils_py_to_r(utils_py$ee_path())

  # setting gcs folder
  email_clean <- gsub("@gmail.com", "", email)
  ee_path_user <- sprintf("%s/%s", ee_path, email_clean)

  # gcs_credentials
  full_credentials <- list.files(path = ee_path_user, full.names = TRUE)
  gcs_condition <- grepl(".json", full_credentials)
  if (!any(gcs_condition)) {
    gcs_text <- paste(
      sprintf("Unable to find a service account key (SAK) file in: %s",  bold(ee_path_user)),
      "Please, download and save the key manually on the path mentioned",
      "before. A compressible tutorial to obtain their SAK file is available in:",
      "> https://r-spatial.github.io/rgee/articles/rgee05.html",
      bold("Until you do not save a SKA file, the following functions will not work:"),
      "- rgee::ee_gcs_to_local()",
      "- ee_extract(..., via = \"gcs\")",
      "- ee_as_raster(..., via = \"gcs\")",
      "- ee_as_stars(..., via = \"gcs\")",
      "- ee_as_sf(..., via = \"gcs\")",
      "- sf_as_ee(..., via = \"gcs_to_asset\")",
      "- gcs_to_ee_image",
      "- raster_as_ee",
      "- local_to_gcs",
      "- stars_as_ee",
      sep = "\n"
    )
    gcs_info <- list(path = NA, message = gcs_text)
    invisible(gcs_info)
  } else {
    gcs_credentials <- full_credentials[gcs_condition]
    googleCloudStorageR::gcs_auth(gcs_credentials)
    unlink(list.files(ee_path, ".json", full.names = TRUE))
    file.copy(
      from = gcs_credentials,
      to = sprintf("%s/%s", ee_path, basename(gcs_credentials)),
      overwrite = TRUE
    )
    gcs_info <- list(path = gcs_credentials, message = NA)
    invisible(gcs_info)
  }
}

#' Display the credentials of all users as a table
#'
#' Display Earth Engine, Google Drive, and Google Cloud Storage Credentials as
#' a table.
#' @family session management functions
#' @return A data.frame with credential information of all users.
#' @param quiet Logical. Suppress info messages.
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_users()
#' }
#' @export
ee_users <- function(quiet = FALSE) {
  #space among columns
  wsc <- "     "
  title  <- c('user', ' EE', ' GCS', ' GD')

  oauth_func_path <- system.file("python/ee_utils.py", package = "rgee")
  utils_py <- ee_source_python(oauth_func_path)

  # get all dirfiles
  ee_path <- ee_utils_py_to_r(utils_py$ee_path()) %>%
    list.dirs(full.names = FALSE) %>%
    '['(-1)

  if (length(ee_path) == 0) {
    stop('does not exist active users',
         ', run rgee::ee_Initialize() to fixed.')
  }

  #define space in the first column
  max_char <- max(nchar(ee_path))
  add_space <- max_char - nchar(ee_path)
  title[1] <- add_extra_space(name = title[1],
                              space = max_char - nchar(title[1]))

  if (!quiet) {
    cat("", bold(paste0(title, collapse = wsc)), "\n")
  }

  users <- add_extra_space(ee_path, add_space)
  for (user in users) {
    if (user == users[1]) {
      first_user_info <- create_table(user, wsc, quiet = quiet)
    } else {
      user_info <- create_table(user, wsc, quiet = quiet)
      first_user_info <- rbind(first_user_info, user_info)
    }
  }

  if(!quiet) {
    cat("\n")
  }

  invisible(first_user_info)
}

#' Display the credentials and general info of the initialized user
#' @family session management functions
#' @return A list with information about the Earth Engine user.
#' @param quiet Logical. Suppress info messages.
#'
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_Initialize()
#' ee_user_info()
#' }
#' @export
ee_user_info <- function(quiet = FALSE) {
  user_session <- ee_get_earthengine_path()
  user_session_list <- list.files(user_session,full.names = TRUE)
  user <- ee$data$getAssetRoots()[[1]][["id"]]

  if (!quiet) {
    cat(rule(right = bold(paste0("Earth Engine user info"))))
  }

  # python version
  py_used <- py_discover_config()[["python"]]
  if (!quiet) {
    cat(blue$bold("\nReticulate python version:"))
    cat("\n - ", py_used)
  }

  # asset home
  asset_home <- ee_remove_project_chr(user)
  if (!quiet) {
    cat(blue$bold('\nEarth Engine Asset Home:'))
    cat("\n - ", asset_home)
  }

  # credentials directory path
  if (!quiet) {
    cat(blue$bold('\nCredentials Directory Path:'))
    cat("\n - ", user_session)
  }

  # google drive
  gd <- user_session_list[grepl("@gmail.com", user_session_list)]
  if (length(gd) == 0) {
    gd <- "NOT FOUND"
  }

  if (!quiet) {
    cat(blue$bold('\nGoogle Drive Credentials:'))
    cat("\n - ", basename(gd))
  }
  email_drive <- sub("[^_]+_(.*)@.*", "\\1", basename(gd))

  # google cloud storage
  gcs <- user_session_list[grepl(".json", user_session_list)]
  if (length(gcs) == 0) {
    gcs <- "NOT FOUND"
  }

  if (!quiet) {
    cat(blue$bold('\nGoogle Cloud Storage Credentials:'))
    cat("\n - ",basename(gcs))
    cat("\n", rule(), "\n")
  }

  # ee_user <- ee_exist_credentials()
  # if (isFALSE(grepl(email_drive, ee_user$email)) & ee_user$email != "ndef") {
  #   message(
  #     "\nNOTE: Google Drive credential does not match with your Google",
  #     " Earth Engine credentials. All functions which depend on Google",
  #     " Drive will not work (e.g. ee_image_to_drive)."
  #   )
  # }

  ee_check_python_packages(quiet = TRUE)
  list(
    asset_home = asset_home,
    user_session = user_session,
    gd_id = basename(gd),
    gcs_file = gcs
  )
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
  df <- data.frame(
    email = email, user = user, drive_cre = drive_cre, gcs_cre = gcs_cre,
    stringsAsFactors = FALSE
  )
  write.table(df, sessioninfo, row.names = FALSE)
}

#' Read and evaluate a python script
#' @noRd
ee_source_python <- function(oauth_func_path) {
  module_name <- gsub("\\.py$", "", basename(oauth_func_path))
  module_path <- dirname(oauth_func_path)
  import_from_path(module_name, path = module_path, convert = FALSE)
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

#' Message for new users
#' @noRd
ee_init_message <- function(debug=FALSE) {
  text <- paste(
    crayon::bold("Welcome to the Earth Engine client library for R!"),
    "----------------------------------------------------------------",
    "It seems it is your first time using rgee. First off, keep in mind that",
    sprintf("Google Earth Engine is %s, check the",
            bold("only available to registered users")),
    sprintf("official website %s to get more information.",
            bold("https://earthengine.google.com/")),
    "\nBefore start coding is necessary to set up a Python environment. Run",
    sprintf(
      "%s to set up automatically, after that, restart the R",
      bold("rgee::ee_install()")
    ),
    "session to see changes. See more than 300+ examples of rgee at",
    crayon::bold("https://r-earthengine.github.io/."),
    "\nrgee will need to create the environmental variable \"EARTHENGINE_INIT_MESSAGE\"",
    "in your system to avoid you recieve this message again.",
    "",
    sep = "\n"
  )
  message(text)
  if (!debug) {
    response <- readline("Would you like to stop to receive this message? [Y/n]:")
  } else {
    response <- "y"
  }
  repeat {
    ch <- tolower(substring(response, 1, 1))
    if (ch == "y" || ch == "") {
      # message("Initialization aborted.")
      ee_install_set_init_message()
      break
    } else if (ch == "n") {
      # message("Initialization aborted.")
      # return(FALSE)
      break
    } else {
      response <- readline("Please answer yes or no: ")
    }
  }
  invisible(TRUE)
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
        "does not have the Python package \"earthengine-api\" installed. Are you restarted/terminated.",
        "your R session after install miniconda or run ee_install()?",
        "If no do it, please try again:",
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

#' Testing 403 error in GD
#' @noRd
test_drive_privileges <-function(user) {
  ee_check_packages("ee_Initialize(..., drive=TRUE)", "gargle")
  # this will break in GD API v4 ... be careful :)
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
          sprintf("ee_clean_credentials('%s')", user)
        )
      )
    )
  }
}
