#' Authenticate and Initialize Earth Engine
#'
#' Authorize rgee to manage Earth Engine resources, Google
#' Drive, and Google Cloud Storage. The \code{ee_initialize()} via
#' web-browser will ask to sign in to your Google account and
#' allows you to grant permission to manage resources. This function is
#' a wrapper around `rgee::ee$Initialize()`.
#'
#' @param email Character (optional, e.g. `data.colec.fbf@gmail.com`). The email
#' name is used as a folder inside the path `rgee::ee_get_earthengine_path()`.
#' This enable a multi-user support allowing to target a specific
#' Google identity.
#'
#' @param drive Logical (optional). If TRUE, the drive credential
#' will be cached in the path `rgee::ee_get_earthengine_path()`.
#' @param gcs Logical. If TRUE, the Google Cloud Storage
#' credential will be cached in the path `rgee::ee_get_earthengine_path()`.
#' @param quiet Logical. Suppress info messages.
#'
#' @importFrom utils read.table browseURL write.table packageVersion
#' @importFrom reticulate import_from_path import install_miniconda py_available
#' @importFrom getPass getPass
#' @importFrom cli symbol rule
#' @importFrom crayon blue green black red bold white
#'
#' @details
#' \code{ee_Initialize(...)} can also manage Google drive and Google
#' Cloud Storage resources using the R packages googledrive and
#' googlecloudStorageR, respectively. By default, rgee does not require
#' them, these are only necessary for exporting and importing tasks.
#' All user credentials are saved in the directory
#' \code{~/.config/earthengine/}, if a user does not specify the email
#' argument all user credentials will be saved in a subdirectory
#' called \code{~/.config/earthengine/ndef}.
#'
#' @family session management functions
#'
#' @examples
#' \dontrun{
#' library(rgee)
#'
#' # Simple init - Load just the Earth Engine credential
#' ee_Initialize()
#'
#' # Advanced init - Load full credentials
#' ee_Initialize(
#'   email = "your_email@gmail.com",
#'   drive = TRUE,
#'   gcs = TRUE
#' )
#'
#' ee_user_info()
#' }
#' @export
ee_Initialize <- function(email = NULL,
                          drive = FALSE,
                          gcs = FALSE,
                          quiet = FALSE) {
  # Message for new user
  init_rgee_message <- Sys.getenv("EARTHENGINE_INIT_MESSAGE", unset = NA)
  if (is.na(init_rgee_message) &&  !py_module_available("ee")) {
    text <- paste(
      crayon::bold("Welcome to the Earth Engine client library for R!"),
      "----------------------------------------------------------------",
      "It seems it is your first time using rgee. Before start coding is ",
      sprintf(
        "necessary to set up a Python environment. Run %s",
        crayon::bold("rgee::ee_install()")
      ),
      "to set up automatically, after that, restart the R session to see",
      "changes. See more than 250+ examples of rgee at",
      crayon::bold("https://csaybar.github.io/rgee-examples/"),
      "",
      sep = "\n"
    )
    message(text)
    response <- readline("Would you like to see this message again? [Y/n]: ")
    repeat {
      ch <- tolower(substring(response, 1, 1))
      if (ch == "y" || ch == "") {
        message("Initialization aborted.")
        return(FALSE)
      } else if (ch == "n") {
        message("Initialization aborted.")
        ee_install_set_init_message()
        return(FALSE)
      } else {
        response <- readline("Please answer yes or no: ")
      }
    }
  }
  # get the path of earth engine credentials
  ee_current_version <- system.file("python/ee_utils.py", package = "rgee")
  ee_utils <- ee_source_python(ee_current_version)
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
    stop(
      "Update local installations to v0.1.215 or greater. ",
      "Earlier versions are not compatible with recent ",
      "changes to the Earth Engine backend."
    )
  }

  # 1. simple checking
  if (is.null(email)) {
    email <- "ndef"
  }

  if (!quiet) {
    if (email == "ndef") {
      cat(
        "", green(symbol$tick),
        blue("email:"),
        green("not_defined\n")
      )
    } else {
      cat(
        "", green(symbol$tick),
        blue("email:"),
        green(email), "\n"
      )
    }
  }

  # create a user's folder
  email_clean <- gsub("@gmail.com", "", email)
  ee_path <- ee_utils_py_to_r(ee_utils$ee_path())
  ee_path_user <- sprintf("%s/%s", ee_path, email_clean)
  dir.create(ee_path_user, showWarnings = FALSE, recursive = TRUE)

  ## remove previous gd and gcs credentials
  unlink(list.files(ee_path, "@gmail.com", full.names = TRUE))
  unlink(list.files(ee_path, ".json", full.names = TRUE))

  # Loading all the credentials: earthengine, drive and GCS.
  drive_credentials <- NA
  gcs_credentials <- NA

  if (drive) {
    if (!quiet) {
      cat(
        "",
        green(symbol$tick),
        blue("Google Drive credentials:")
      )
    }
    drive_credentials <- ee_create_credentials_drive(email)
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
    if (!quiet) {
      cat(
        "",
        green(symbol$tick),
        blue("GCS credentials:")
      )
    }
    gcs_credentials <- ee_create_credentials_gcs(email)
    if (!quiet) {
      cat(
        "\r",
        green(symbol$tick),
        blue("GCS credentials:"),
        # gcs_credentials,
        green(" FOUND\n")
      )
    }
  }
  ## rgee session file
  options(rgee.gcs.auth = gcs_credentials)
  options(rgee.manage.setIamPolicy = list(bindings = list(
    list(
      role = "roles/owner",
      members = paste0("user:", email)
    ),
    list(
      role = "roles/editor",
      members = NULL
    ),
    list(
      role = "roles/viewer",
      members = NULL
    )
  )))
  if (!quiet) {
    cat(
      "", green(symbol$tick),
      blue("Initializing Google Earth Engine:")
    )
  }

  ee_create_credentials_earthengine(email_clean)
  ee$Initialize()

  if (!quiet) {
    cat(
      "\r",
      green(symbol$tick),
      blue("Initializing Google Earth Engine:"),
      green(" DONE!\n")
    )
  }

  # Root folder exist?
  ee_user <- tryCatch(
    expr = ee_remove_project_chr(ee$data$getAssetRoots()[[1]]$id),
    error = function(e) stop(
      "Earth Engine Assets home root folder does not ",
      "exist for the current user. ",
      sprintf(
        "Run %s to attempt to create it. ",
        bold("ee$data$createAssetHome(users/PUT_YOUR_NAME_HERE)")
      ),
      sprintf(
        "Take into consideration that once created %s",
        bold("you will not be able to change the folder name again. ")
      ),
      "If the root folder was created successfully ",
      sprintf("execute again %s.", bold("ee_Initialize()"))
    )
  )

  options(rgee.ee_user = ee_user)
  ee_sessioninfo(
    email = email_clean,
    user = ee_user,
    drive_cre = drive_credentials,
    gcs_cre = gcs_credentials
  )

  if (!quiet) {
    cat("\r", green(symbol$tick), blue("Earth Engine user:"),
        green(bold(ee_user)), "\n")
    cat(rule(), "\n")
  }
  # ee_check_python_packages(quiet = TRUE)
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
#' will download from Internet, you will be directed to a web browser.
#' Sign in to your Google account to be granted rgee
#' permission to operate on your behalf with Google Earth Engine.
#' These user credentials are cached in a folder below your
#' home directory, `rgee::ee_get_earthengine_path()`, from
#' where they can be automatically refreshed, as necessary.
#' }
#' @noRd
ee_create_credentials_earthengine <- function(email_clean) {
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
    browseURL(earthengine_auth)
    auth_code <- getPass("Enter Earth Engine Authentication: ")
    token <- ee$oauth$request_token(auth_code, code_verifier)
    credential <- sprintf('{"refresh_token":"%s"}', token)
    write(credential, main_ee_credential)
    write(credential, user_ee_credential)
  }
}

#' Create credentials - Google Drive
#' @noRd
ee_create_credentials_drive <- function(email) {
  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop("The googledrive package is not installed. ",
      'Try install.packages("googledrive")',
      call. = FALSE
    )
  }
  # setting drive folder
  oauth_func_path <- system.file("python/ee_utils.py", package = "rgee")
  utils_py <- ee_source_python(oauth_func_path)
  ee_path <- ee_utils_py_to_r(utils_py$ee_path())
  email_clean <- gsub("@gmail.com", "", email)
  ee_path_user <- sprintf("%s/%s", ee_path, email_clean)
  # drive_credentials
  repeat {
    full_credentials <- list.files(path = ee_path_user, full.names = TRUE)
    drive_condition <- grepl("@gmail.com", full_credentials)
    if (!any(drive_condition)) {
      suppressMessages(
        googledrive::drive_auth(
          email = NULL,
          cache = ee_path_user
        )
      )
    } else {
      drive_credentials <- full_credentials[drive_condition]
      email <- sub("^[^_]*_", "", drive_credentials)
      suppressMessages(
        googledrive::drive_auth(
          email = email,
          cache = ee_path_user
        )
      )
      break
    }
  }
  # from user folder to EE folder
  unlink(list.files(ee_path, "@gmail.com", full.names = TRUE))
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
#' (https://console.cloud.google.com/apis/credentials/serviceaccountkey).
#' Is necessary to save it (manually) inside the folder ~/.R/earthengine/USER/.
#' @noRd
ee_create_credentials_gcs <- function(email) {
  if (!requireNamespace("googleCloudStorageR", quietly = TRUE)) {
    stop("The googleCloudStorageR package is not installed. Try",
      ' install.packages("googleCloudStorageR")',
      call. = FALSE
    )
  }
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
    stop(
      "Unable to find GCS_AUTH_FILE.json in ", ee_path_user, ". \n",
      "First download and save the Google Project JSON file in ",
      "the path mentioned before.\n",
      "A compressible tutorial to obtain the GCS_AUTH_FILE.json is ",
      "available here: \n",
      "- https://github.com/csaybar/GCS_AUTH_FILE.json\n",
      "- https://console.cloud.google.com/apis/credentials/serviceaccountkey ",
      "(download link)\n"
    )
  } else {
    gcs_credentials <- full_credentials[gcs_condition]
    googleCloudStorageR::gcs_auth(gcs_credentials)
  }
  unlink(list.files(ee_path, ".json", full.names = TRUE))
  file.copy(
    from = gcs_credentials,
    to = sprintf("%s/%s", ee_path, basename(gcs_credentials)),
    overwrite = TRUE
  )
  invisible(gcs_credentials)
}

#' Display the credentials of all users as a table
#'
#' Display Earth Engine, Google Drive, and Google Cloud Storage Credentials as
#' a table.
#' @family session management functions
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
  title  <- c('user', ' EE', ' GD', ' GCS')

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
    create_table(user, wsc, quiet = quiet)
  }

  if(!quiet) {
    cat("\n")
  }

  invisible(TRUE)
}

#' Display the credentials and general info of the initialized user
#' @family session management functions
#'
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
  user <- ee$data$getAssetRoots()[[1]]$id

  if (!quiet) {
    cat(rule(right = bold(paste0("Earth Engine user info"))))
  }

  # python version
  py_used <- py_discover_config()$python
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
  if (!quiet) {
    cat(blue$bold('\nGoogle Drive Credentials:'))
    cat("\n - ", basename(gd))
  }
  email_drive <- sub("[^_]+_(.*)@.*", "\\1", basename(gd))

  # google cloud storage
  gcs <- user_session_list[grepl(".json", user_session_list)]
  if (!quiet) {
    cat(blue$bold('\nGoogle Cloud Storage Credentials:'))
    cat("\n - ",basename(gcs))
    cat("\n", rule(), "\n")
  }
  ee_user <- ee_exist_credentials()

  if (isFALSE(grepl(email_drive, ee_user$email)) & ee_user$email != "ndef") {
    message(
      "\nNOTE: Google Drive credential does not match with your Google",
      " Earth Engine credentials. All functions which depend on Google",
      " Drive will not work (e.g. ee_image_to_drive)."
    )
  }
  ee_check_python_packages(quiet = TRUE)
  invisible(TRUE)
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

#' Get the path where the credentials are stored
#'
#' @family path utils
#' @return A character which represents the path credential of a specific
#' user
#'
#' @export
ee_get_earthengine_path <- function() {
  oauth_func_path <- system.file("python/ee_utils.py", package = "rgee")
  utils_py <- ee_source_python(oauth_func_path)
  ee_path <- ee_utils_py_to_r(utils_py$ee_path())

  sessioninfo <- sprintf(
    "%s/rgee_sessioninfo.txt",
    ee_utils_py_to_r(utils_py$ee_path())
  )
  if (file.exists(sessioninfo)) {
    user <- read.table(sessioninfo,
      header = TRUE,
      stringsAsFactors = FALSE
    )[[1]]
    if (is.na(user)) {
      stop("rgee_sessioninfo.txt malformed")
    }
  } else {
    stop(
      "rgee_sessioninfo.txt does not exist, ",
      "run rgee::ee_Initialize() to fixed."
    )
  }
  return(sprintf("%s/%s/", ee_path, user))
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
    gmail_symbol <- green(symbol$tick)
  } else {
    gmail_symbol <- red(symbol$cross)
  }

  #GCS
  if (any(grepl(".json",credentials))) {
    gcs_symbol <- green(symbol$tick)
  } else {
    gcs_symbol <- red(symbol$cross)
  }

  #Earth Engine
  if (any(grepl("credentials",credentials))) {
    ee_symbol <- green(symbol$tick)
  } else {
    ee_symbol <- red(symbol$cross)
  }

  if (!quiet) {
    cat("\n",
        user,
        wsc,
        gmail_symbol,
        wsc,
        gcs_symbol,
        wsc,
        ee_symbol
      )
  }
}

#' Get the Asset home name
#' @family path utils
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_Initialize()
#'
#' ee_get_assethome()
#' }
#' @return Character. The name of the Earth Engine Asset home
#' (e.g. users/datacolecfbf)
#' @export
ee_get_assethome <- function() {
  options('rgee.ee_user')[[1]]
}
