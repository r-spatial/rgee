#' Authenticate and Initialize Earth Engine
#'
#' Authorize rgee to manage Earth Engine resource The \code{ee_initialize()} via web-browser asked to sign in
#' to your Google account and grant permission for managing google earth engine. This function is a wrapper around
#' `rgee::ee$Initialize()`.
#'
#' @param user_gmail 	optional. Allows user to target a specific Google identity.
#' @param drive logical. If TRUE drive credential are cached in the system.
#' @param gcs logical. If TRUE drive credential are cached in the system.
#' @param assethome character. Home folder's names of you EE Assets.
#' @param quiet logical. Suppress info messages.
#' @param checkpy logical. Check if the current Python version of this system is admitted by rgee.
#' @importFrom utils read.table browseURL write.table
#' @importFrom reticulate import_from_path import
#' @import getPass getPass
#' @details
#' \code{ee_Initialize()} give the possibility of authorize Google drive (googledrive) and Google Cloud
#'  Storage (googlecloudStorageR). By default, rgee do not need to them, these are just neccesary for
#'  exportation and importation tasks. All the user credentials are save in the folder \code{~/.config/earthengine/USERS/},
#'  if a user is not specified the parameters of the last session will be used.
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_reattach() # reattach ee as a reserved word
#' ee_Initialize()
#' }
#' @export
ee_Initialize <- function(user_gmail = NULL,
                          drive = FALSE,
                          gcs = FALSE,
                          assethome = NULL,
                          checkpy = FALSE,
                          quiet = FALSE) {
  ee_reattach()
  if (checkpy) ee_check_python(quiet=quiet)
  list_ids <- ee_get_asset_gmail(user_gmail, assethome)
  user_gmail <- list_ids$user_gmail
  assethome <- gsub("\\.", "", list_ids$assethome)[1]

  if (!is.null(assethome) & !is.null(user_gmail)) {
    cat("user_gmail:",user_gmail,"\n")
    cat("assethome:",assethome,"\n")
  }

  if (!is.na(assethome)) earthengine <- TRUE else earthengine <- FALSE

  #Function core
  if (earthengine) {
    ee_create_credentials_earthengine(assethome)
  }
  if (drive) {
    ee_create_credentials_drive(user_gmail, assethome, quiet)
  }
  if (gcs) {
    ee_create_credentials_gcs(assethome, quiet)
  }

  tryCatch(expr = ee$Initialize(),
           error = function(e) {
             ee_create_credentials_earthengine(assethome)
             })

  # Check assethome sanity
  if (!is.null(assethome) & !is.null(user_gmail)) {
    cat('Checking correspondence between user_gmail and assethome ...')
    #ee_reattach()
    home_error <- tryCatch(expr = ee$data$getAssetRoots()[[1]]$'id',
                error = function(e){
                  cat("\n")
                  stop("Earth Engine ID no found, try:",
                       "\nee$data$createAssetHome('...'): Create the EE assets home folder",
                       "\nee_reattach(): Reattach ee as a reserved word")
                })
    if (!identical(assethome, home_error)) {
      stop("The Earth Engine credentials obtained from the user_gmail",
           " does not correspond with the assethome,",
           " define properly the arguments user_gmail and assethome to fixed.")
    }
    cat(" DONE \n")
  }

  ee_path <- path.expand("~/.config/earthengine")
  clean_user <- gsub("users/","",assethome)[1]
  ee_path_user <- sprintf("%s/%s",ee_path, clean_user)
  drive_credentials <- list.files(ee_path_user,"@gmail.com",full.names = TRUE)[1]
  gcs_credentials <- list.files(ee_path_user,"GCS_AUTH_FILE.json",full.names = TRUE)[1]
  ee_sessioninfo(clean_user,drive_credentials, gcs_credentials)

  options(rgee.selenium.params=list(user_gmail=user_gmail,
                                    user_password=Sys.getenv("PASSWORD_GMAIL"),
                                    showpassword=FALSE))

  invisible(TRUE)
}

#' Authorize rgee
#'
#' Authorize rgee to view and manage your Earth Engine account.
#'
#' By default, you are directed to a web browser, asked to sign in to your Google
#' account, and to grant rgee permission to operate on your behalf with Google Earth
#' Engine. By default, these user credentials are cached in a folder below your home
#' directory, ~/.R/earthengine/USER/credentials, from where they can be automatically
#' refreshed, as necessary.
#' @noRd
ee_create_credentials_earthengine <- function(assethome) {
  ee_path <- path.expand("~/.config/earthengine")
  main_ee_credential <- sprintf("%s/credentials",ee_path)
  user_ee_credential <- sprintf("%s/%s/credentials",
                                ee_path,
                                gsub("users/","",assethome))
  if (length(file.exists(user_ee_credential)) == 0) path_condition = FALSE else path_condition = TRUE

  copy_cre <- FALSE
  if (path_condition) {
    copy_cre <- file.copy(from = user_ee_credential,
                          to = main_ee_credential,
                          overwrite = TRUE)
  }

  if (!copy_cre | !path_condition) {
    earthengine_auth <- ee$oauth$get_authorization_url()
    browseURL(earthengine_auth)
    auth_code <- getPass("Enter Earth Engine Authentication: ")
    token <- ee$oauth$request_token(auth_code)
    credential <- sprintf('{"refresh_token":"%s"}',token)
    if (is.null(assethome)) {
      write(credential, main_ee_credential)
    } else {
      path_name <- sprintf("%s/%s/", ee_path, gsub("users/","",assethome))
      dir.create(path_name, showWarnings = FALSE,recursive = TRUE)
      write(credential, main_ee_credential)
      write(credential, user_ee_credential)
    }
  }
}

#' Create credentials Google Drive
#' @noRd
ee_create_credentials_drive <- function(user_gmail, assethome, quiet) {
  if (!requireNamespace('googledrive', quietly = TRUE)) {
    stop('The googledrive package is not installed. Try install.packages("googledrive")',
         call. = FALSE)
  }
  if (!quiet) cat('Getting Google Drive credentials ... ')
  ee_path <- path.expand("~/.config/earthengine")
  if (is.null(assethome)) {
    ee_path_user <- sprintf("%s/",ee_path)
    suppressMessages(googledrive::drive_auth(email = TRUE, cache = ee_path_user))
  } else {
    ee_path_user <- sprintf("%s/%s/",ee_path, gsub("users/","",assethome))[1]
    suppressMessages(googledrive::drive_auth(email = user_gmail, cache = ee_path_user))
    user_drive_credential <- list.files(ee_path_user, "@gmail.com", full.names = TRUE)[1]
    unlink(list.files(ee_path, "@gmail.com", full.names = TRUE))
    main_drive_credential <- sprintf("%s/%s", ee_path, basename(user_drive_credential))
    copy_cre <- file.copy(from = user_drive_credential,
                          to = main_drive_credential,
                          overwrite = TRUE)
  }
  if (!quiet) cat(' DONE\n')
}


#' Authorize Google Cloud Storage
#'
#' Authorize Google Cloud Storage to view and manage your gcs files.
#'
#' @param save_credentials Path to save credentials
#'@details
#' GCS_AUTH_FILE.json is the authentication json file you have downloaded from your Google Project
#' (https://console.cloud.google.com/apis/credentials/serviceaccountkey). Is necessary to save it (manually)
#' inside the folder ~/.R/earthengine/USER/.
#' @noRd
ee_create_credentials_gcs <- function(assethome, quiet) {
  if (!requireNamespace('googleCloudStorageR', quietly = TRUE)) {
    stop('The googleCloudStorageR package is not installed. Try',
         ' install.packages("googleCloudStorageR")',
         call. = FALSE)
  }
  if (!quiet) cat('Checking Google Cloud storage credentials ... ')
  ee_path <- path.expand("~/.config/earthengine/")
  main_gcs_credential <- sprintf("%sGCS_AUTH_FILE.json", ee_path)
  user_gcs_credential <- sprintf("%s/%s/GCS_AUTH_FILE.json",
                                 ee_path,
                                 gsub("users/","",assethome))

  if (length(file.exists(user_gcs_credential)) == 0) path_condition = FALSE else path_condition = TRUE
  copy_cre <- FALSE
  if (path_condition) {
    unlink(main_gcs_credential)
    copy_cre <- file.copy(from = user_gcs_credential,
                          to = main_gcs_credential,
                          overwrite = TRUE)
  }
  ee_get_credentials_gcs(main_gcs_credential, quiet)
}

#' Create session info inside the folder ~/.config/earthengine/USERS/
#' @noRd
ee_sessioninfo <- function(user = NULL,drive_cre=NULL, gcs_cre = NULL) {
  sessioninfo <- sprintf("%s/rgee_sessioninfo.txt", path.expand("~/.config/earthengine"))
  df <- data.frame(user = user, drive_cre = drive_cre, gcs_cre = gcs_cre,stringsAsFactors = FALSE)
  write.table(df, sessioninfo,row.names = F)
}


#' Get the path where rgee save the credentials
#' @export
ee_get_earthengine_path <- function() {
  ee_path <- path.expand("~/.config/earthengine")
  sessioninfo <- sprintf("%s/rgee_sessioninfo.txt", path.expand("~/.config/earthengine"))
  if (file.exists(sessioninfo)) {
    user <- read.table(sessioninfo,header = TRUE,stringsAsFactors = FALSE)[[1]]
    if (is.na(user)) {
      stop("rgee_sessioninfo.txt malformed")
    }
  } else {
    stop("rgee_sessioninfo.txt does not exist, run rgee::ee_Initialize) to fixed")
  }
  return(sprintf("%s/%s/",ee_path,user))
}


#' Create user_gmail when just assethome be defined and vice-versa
#' @noRd
ee_get_asset_gmail <- function(user_gmail = NULL, assethome = NULL){
  if (is.null(user_gmail)) {
    if (is.null(assethome)) {
      return(list(user_gmail = user_gmail, assethome = assethome))
    } else {
      assethome = ee_verify_filename(assethome, strict = FALSE)
      user_gmail = sprintf("%s@gmail.com",gsub("users/","",assethome))
      return(list(user_gmail = user_gmail, assethome = assethome))
    }
  } else {
    if (is.null(assethome)) {
      assethome <- sprintf("users/%s",gsub("@gmail.com","",user_gmail))
      return(list(user_gmail = user_gmail, assethome = assethome))
    } else {
      return(list(user_gmail = user_gmail, assethome = assethome))
    }
  }
}

#' Credentials GCS
#' @noRd
ee_get_credentials_gcs <- function(GCScredential, quiet){
  if (!file.exists(GCScredential)) {
    cat("\n")
    stop(
         "Unable to find GCS_AUTH_FILE.json in ", GCScredential,". \n",
         "Firstly download and save the Google Project JSON file in the path mentioned before.\n",
         "A compressible tutorial for obtaining GCS_AUTH_FILE.json is available here: \n",
         "- https://github.com/csaybar/GCS_AUTH_FILE.json\n",
         "- https://console.cloud.google.com/apis/credentials/serviceaccountkey (download link)\n"
    )
  } else {
    googleCloudStorageR::gcs_auth(GCScredential)
  }
  if (!quiet) cat(' DONE\n')
}

#' Read and evaluate a python script
#' @noRd
ee_source_python <- function(oauth_func_path) {
  module_name <- gsub("\\.py$","",basename(oauth_func_path))
  module_path <- dirname(oauth_func_path)
  ee_module <- import_from_path(module_name, path = module_path, convert = F)
}
