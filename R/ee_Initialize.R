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
# @param display Logical. If TRUE, display the earthengine authentication URL.
# (display url - useul for colab noteboks)
#' @param credentials  OAuth2 GEE credentials. 'persistent' (default) means
#' use GEE credentials already stored in the filesystem, or raise an explanatory
#' exception guiding the user to create those credentials.
#' @param opt_url The base url for the EarthEngine REST API to connect to.
#' @param cloud_api_key An optional API key to use the Cloud API.
#' @param http_transport The http transport method to use when making requests.
#' @param project The client project ID or number to use when making API calls.
#' @param quiet Logical. Suppress info messages.
#' @param auth_quiet Logical. \link{ee_Authenticate} quiet parameter. If TRUE,
#' do not require interactive prompts and force --no-browser mode for gcloud.
#' @param auth_mode The authentication mode. One of:
#' \itemize{
#'  \item{1. }{paste - send user to accounts.google.com to get a pastable token}
#'  \item{2. }{notebook - send user to notebook authenticator page}
#'  \item{3. }{gcloud - use gcloud to obtain credentials (will set appdefault)}
#'  \item{4. }{appdefault - read from existing $GOOGLE_APPLICATION_CREDENTIALS file}
#'  \item{5. }{None - a default mode is chosen based on your environment.}
#' }
#' @param email User email (Optional)
#' @param use_oob Should out of band authentication be used for Google Drive?
#' Default is FALSE.
#' @param drive_cred_path Optional path to google drive credentials JSON key (Default is NULL)
#' @param gcs_cred_path Optional path to google cloud storage credentials JSON key (Default is NULL)
#' @param ee_cred_path Optional path to earth engine JSON key (Default is NULL)
#' @param ... Extra exporting argument. See \link{ee_Authenticate}.
#'
#' @importFrom utils read.table browseURL write.table packageVersion
#' @importFrom reticulate import_from_path import install_miniconda py_available
#' @importFrom cli symbol rule
#' @importFrom crayon blue green black red bold white
#'
#' @details
#' \code{ee_Initialize()} can manage Google Drive, and Google
#' Cloud Storage resources using the R packages googledrive and
#' googlecloudStorageR, respectively. By default, rgee does not require
#' them. These are only necessary to enable rgee I/O functionality.
#' All user credentials are saved in the directory
#' \code{~/.config/earthengine/}.
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
                          credentials='persistent',
                          opt_url=NULL,
                          cloud_api_key=NULL,
                          http_transport=NULL,
                          project=NULL,
                          quiet = FALSE,
                          auth_mode = "notebook",
                          auth_quiet = FALSE,
                          email = NULL,
                          drive_path = NULL,
                          use_oob = FALSE,
                          drive_cred_path = NULL,
                          gcs_cred_path = NULL,
                          ee_cred_path = NULL,
                          ...
) {

  # Set google-cloud-sdk in your PATH systen
  gcloud_path <- Sys.getenv("EARTHENGINE_GCLOUD", unset = NA)

  #If provided, set GCS path
    if(!is.null(gcs_cred_path)){
      Sys.setenv("GCS_AUTH_FILE" = gcs_cred_path)
    }

  if (!is.na(gcloud_path)){
    Sys.setenv(PATH = sprintf("%s:%s", Sys.getenv("PATH"), gcloud_path))
  }

  # Check sanity of earth-engine and return ee_utils.py module
  init <- ee_check_init()
  ee_utils <- init$ee_utils

  # rgee init message
  if (!quiet){ ee_message_01(user, init$earthengine_version)}

  # If user is not NULL create, then save the credentials in a subfolder.
  if (!is.null(user)) {
    # Create user folder is it does not exist
    ee_create_user_subfolder(ee_utils, user)

    # delete previous user credentials
    delete_credentials(ee_utils)
  }

  # Loading all the credentials: earthengine, drive and GCS.
  drive_credentials <- NA
  gcs_credentials <- list(path = NA, message = NA)

  if (drive) {
    ee_check_packages("ee_Initialize", "googledrive")

    # drive init message
    if (!quiet){ ee_message_02(init = TRUE)}

    # If the user is not NULL copy the drive credential in the subfolder
    drive_credentials <- ee_create_credentials_drive(user,
                                                     ee_utils,
                                                     quiet = quiet,
                                                     drive_cred_path = drive_cred_path,
                                                     use_oob = use_oob)
    test_drive_privileges(user)

    if (!quiet){ ee_message_02(init = FALSE)}
  }

  if (gcs) {
    ee_check_packages("ee_Initialize", "googleCloudStorageR")

    if (!quiet){ ee_message_03(init=TRUE, gcs_credentials)}

    # Load GCS credentials
    gcs_credentials <- ee_create_credentials_gcs(user, ee_utils)

    if (!quiet){ ee_message_03(init=FALSE, gcs_credentials)}
  }

  ## rgee session file
  options(rgee.gcs.auth = gcs_credentials[["path"]])

  if (!quiet){ ee_message_04(init = TRUE)}

  # If user is not NULL copy the credentials from sub to main folder
  ee_create_credentials_earthengine(user,
                                    auth_mode,
                                    auth_quiet,
                                    ee_utils,
                                    ee_cred_path = ee_cred_path
                                    #, ...
                                    )

  tryCatch(expr = {
    ee$Initialize(
      credentials=credentials,
      opt_url=opt_url,
      cloud_api_key=cloud_api_key,
      http_transport=http_transport,
      project=project
    )
  }, error = function(e) {
     if (grepl("Token has been expired", e)) {
       ee_Authenticate(
         user = user,
         earthengine = TRUE
       )
       ee$Initialize(
         credentials=credentials,
         opt_url=opt_url,
         cloud_api_key=cloud_api_key,
         http_transport=http_transport,
         project=project
       )
     }
  })

  if (!quiet){ ee_message_04(init = FALSE)}

  # check if the GEE acount has been created a GEE mainfolder

    ee_user <- ee_check_root_folder()

  options(rgee.ee_user = ee_user)

  ee_sessioninfo(
    email = if(!is.null(email)){email}else{if (is.null(user)){ "ndef" }else{ user}},
    user = ee_user,
    drive_cre = drive_credentials$drive_credentials,
    gcs_cre = gcs_credentials[["path"]],
    drive_cred_path = drive_credentials$drive_cred_path
  )

  if (!quiet){ ee_message_06(gcs_credentials, ee_user)}

  # Add Dataset attribute
  eeDataset <- jsonlite::read_json(system.file("dataset.json", package="rgee"))
  eeDataset_b <- ee_Dataset_creator(eeDataset)

  ee$FeatureCollection$Dataset <- eeDataset_b$fc
  ee$ImageCollection$Dataset <- eeDataset_b$ic
  ee$Image$Dataset <- eeDataset_b$image

  invisible(TRUE)
}


#' Prompts the user to authorize access to Earth Engine via OAuth2.
#' @param user Character (optional). If is a character, the credentials are saved in
#' the dirpath: ~/.config/earthengine/$user. If is NULL, the credentials are stored
#' in ~/.config/earthengine.
#' @param earthengine Logical (optional). If TRUE, the EarthEngine credential
#' is cached in the path \code{~/.config/earthengine/}.
#' @param drive Logical (optional). If TRUE, the drive credential
#' is cached in the path \code{~/.config/earthengine/}.
#' @param gcs Logical (optional). If TRUE, the Google Cloud Storage
#' credential is cached in the path \code{~/.config/earthengine/}.
#' @param authorization_code An optional authorization code.
#' @param code_verifier PKCE verifier to prevent auth code stealing.
#' @param auth_mode The authentication mode. One of:
#' \itemize{
#'  \item{1. }{paste - send user to accounts.google.com to get a pastable token}
#'  \item{2. }{notebook - send user to notebook authenticator page}
#'  \item{3. }{gcloud - use gcloud to obtain credentials (will set appdefault)}
#'  \item{4. }{appdefault - read from existing $GOOGLE_APPLICATION_CREDENTIALS file}
#'  \item{5. }{None - a default mode is chosen based on your environment.}
#' }
#' @param scopes List of scopes to use for authentication. Defaults to
#' 'https://www.googleapis.com/auth/earthengine' or
#' 'https://www.googleapis.com/auth/devstorage.full_control'
#' @param quiet If TRUE, do not require interactive prompts and force --no-browser mode for gcloud.
#' @param verbose Logical. Suppress info messages.
#' @examples
#' \dontrun{
#' library(rgee)
#'
#' # Simple init - Load just the Earth Engine credential
#' ee_Authenticate()
#'
#' # At Server side
#' ee_Authenticate(quiet=TRUE)
#'
#' }
#' @export
ee_Authenticate <- function(
    user=NULL,
    earthengine = TRUE,
    drive = FALSE,
    gcs = FALSE,
    authorization_code = NULL,
    code_verifier = NULL,
    auth_mode = "notebook",
    scopes = NULL,
    quiet = FALSE,
    verbose = TRUE) {

  # Set google-cloud-sdk in your PATH system
  sdkpath <- sprintf("%s/google-cloud-sdk/bin/", Sys.getenv("HOME"))
  if (!grepl(sdkpath, Sys.getenv("PATH"))) {
    Sys.setenv(PATH = sprintf("%s:%s", Sys.getenv("PATH"), sdkpath))
  }

  # Check sanity of earth-engine and return ee_utils.py module
  init <- ee_check_init()
  ee_utils <- init$ee_utils

  # setting the user and main ee folder
  if (is.null(user)) {
    ee_path <- ee_utils_py_to_r(ee_utils$ee_path())
    ee_path_user <- ee_path
  } else {
    ee_path <- ee_utils_py_to_r(ee_utils$ee_path())
    ee_path_user <- sprintf("%s/%s", ee_path, user)
  }


  # Create empty user folder is it does not exist
  if (!is.null(user)) {
    ee_create_user_subfolder(ee_utils, user)
  }

  # Retrieve the Earth Engine credentials
  if (earthengine) {
    # Remove previous credential
    unlink(sprintf("%s/credentials", ee_path_user))

    # Display info message
    if (verbose) ee_message_04(init = TRUE)

    # Extra auth params
    auth_params <- list(
      authorization_code = authorization_code,
      code_verifier = code_verifier,
      scopes = scopes
    )

    # Copy credentials
    ee_create_credentials_earthengine(
      user,
      auth_mode,
      quiet,
      ee_utils,
      auth_params
    )

    # If no error is returned, print ok info message
      if (verbose) ee_message_04(init = FALSE)
  }

  # Retrieve the Drive credentials
  if (drive) {
    ee_check_packages("ee_Initialize", "googledrive")

    # Remove previous credential
    full_credentials <- list.files(path = ee_path_user, full.names = TRUE)
    drive_condition <- grepl(".*_.*@.*", basename(full_credentials))
    unlink(full_credentials[drive_condition])

    # drive init message
    if (verbose) ee_message_02(init = TRUE)

    # Create the drive credential
    drive_credentials <- ee_create_credentials_drive(user, ee_utils, quiet = verbose)
    test_drive_privileges(user)

    if (verbose) ee_message_02(init = FALSE)
  }

  # Retrieve the GCS credentials
  if (gcs) {
    ee_check_packages("ee_Initialize", "googleCloudStorageR")

    # gcs init message
    if (verbose) ee_message_03(init = TRUE, gcs_credentials)


    # Create the gcs credential
    gcs_credentials <- ee_create_credentials_gcs(user=user, ee_utils)

    if (verbose) {
      ee_message_03(init=FALSE, gcs_credentials = gcs_credentials)
      if (is.na(gcs_credentials[["path"]])) {
          message(gcs_credentials[["message"]])
      }
    }
  }

  if (verbose) {
    message("credentials are cached in the path: ", ee_path_user)
  }

  invisible(TRUE)
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
  title  <- c('user', ' EE', ' GD', ' GCS')

  oauth_func_path <- system.file("python/ee_utils.py", package = "rgee")
  utils_py <- ee_source_python(oauth_func_path)

  # get all dirfiles
  ee_path <- ee_utils_py_to_r(utils_py$ee_path()) %>%
    list.dirs(full.names = FALSE) %>%
    '['(-1)

  if (length(ee_path) == 0) {
    stop('does not exist active users')
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
