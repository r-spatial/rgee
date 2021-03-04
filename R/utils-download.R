#' Monitoring Earth Engine task progress
#'
#' @param task List generated after a task is started (i.e., after run
#' `ee$batch$Task$start()`) or a character that represents the ID of a EE
#' task started.
#' @param task_time Numeric. How often (in seconds) should a task be polled?
#' @param eeTaskList Logical. If \code{TRUE}, all Earth Engine tasks will be
#' listed.
#' @param quiet Logical. Suppress info message
#' @return An \code{ee$batch$Task} object with a state "COMPLETED" or "FAILED"
#' according to the Earth Engine server's response.
#' @family helper functions
#' @export
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_Initialize()
#' ee_monitoring(eeTaskList = TRUE)
#' }
#' @export
ee_monitoring <- function(task, task_time = 5, eeTaskList = FALSE, quiet = FALSE) {
  # if task is missing
  if (missing(task)) {
    all_task <- ee_utils_py_to_r(ee$batch$Task$list())
    task <- all_task[[1]]
  }

  # if task is character(ID)
  if (is.character(task)) {
    all_task <- ee_utils_py_to_r(ee$batch$Task$list())
    id_tasks <- lapply(all_task, function(task) task[["id"]]) %>% unlist()
    if (any(id_tasks %in% task)) {
      task <- all_task[[which(id_tasks %in% task)]]
    } else {
      stop("Undefined Task ID entered")
    }
  }

  # List all the EE tasks
  if (eeTaskList) {
    if (!quiet) {
      cat("EETaskList:\n")
    }
    task_list <- mapply(function(x) {
      sprintf("<Task %s: %s (%s)>", x$task_type, x$config, x$state)
    }, ee$batch$Task$list())
    if (!quiet) {
      cat("", paste0(task_list, "\n"))
      cat("\n")
    }
  }

  # Start to monitoring the task ...
  counter <- 0
  while (ee_utils_py_to_r(ee$batch$Task$active(task)) &
         task[["state"]] != "CANCEL_REQUESTED") {
    if (!quiet) {
      cat(sprintf("Polling for task <id: %s, time: %ds>.\n",
                  task[["id"]], counter))
    }
    counter <- counter + task_time
    Sys.sleep(task_time)
  }
  task_status <- ee_utils_py_to_r(ee$batch$Task$status(task))
  if (!quiet) {
    cat(sprintf("State: %s\n", task_status[["state"]]))
  }
  if (task_status[["state"]] != "COMPLETED") {
    message(
      "ERROR in Earth Engine servers: ",
      task_status[["error_message"]]
    )
    stop("ee_monitoring was forced to stop before getting results")
  }
  invisible(task)
}

#' Sort google drives files
#' @noRd
sort_drive_files <- function(drive_files, fileformat) {
  if (fileformat == "SHP") {
    shp_file <- grep(
      pattern = "(\\.prj)|(\\.dbf)|(\\.shp)|(\\.shx)",
      x = drive_files[["name"]]
    )
    selected_drive_files <- drive_files[shp_file, ]
    drive_files_sort <- selected_drive_files[order(selected_drive_files$name), ]
  } else {
    drive_files_sort <- drive_files[order(drive_files[["name"]]), ]
  }
  drive_files_sort
}

#' Sort local files
#' @noRd
ee_sort_localfiles <- function(filenames, fileformat) {
  if (fileformat == "SHP") {
    shp_file <- grep("(\\.prj)|(\\.dbf)|(\\.shp)|(\\.shx)", filenames)
    shp_file <- filenames[shp_file]
    shp_file[order(shp_file)]
  } else {
    filenames[order(filenames)]
  }
}


#' (GCS or Google Drive) Exist external credentials?
#' @noRd
ee_exist_credentials <- function() {
  oauth_func_path <- system.file("python/ee_utils.py", package = "rgee")
  utils_py <- ee_source_python(oauth_func_path)
  ee_path <- ee_utils_py_to_r(utils_py$ee_path())
  read.table(
    file = sprintf("%s/rgee_sessioninfo.txt", ee_path),
    header = TRUE,
    stringsAsFactors = FALSE
  )
}

#' Save external credentials
#' @noRd
ee_save_credential <- function(pdrive = NULL, pgcs = NULL) {
  oauth_func_path <- system.file("python/ee_utils.py", package = "rgee")
  utils_py <- ee_source_python(oauth_func_path)
  ee_path <- ee_utils_py_to_r(utils_py$ee_path())
  sessioninfo <- sprintf("%s/rgee_sessioninfo.txt", ee_path)
  cre_table <- read.table(
    file = sprintf("%s/rgee_sessioninfo.txt", ee_path),
    header = TRUE,
    stringsAsFactors = FALSE
  )
  if (!is.null(pdrive)) {
    cre_table[["drive_cre"]] <- pdrive
  } else if (!is.null(pgcs)) {
    cre_table[["gcs_cre"]] <- pgcs
  }
  write.table(cre_table, sessioninfo, row.names = FALSE)
}

#' type of an Earth Engine Image
#' @noRd
ee_get_typeimage_size <- function(type) {
  if (type == "int") {
    32
  } else if (type == "INT") {
    32
  } else if (type == "double") {
    64
  } else if (type == "float") {
    64
  } else if (type == "int8") {
    8
  } else if (type == "int16") {
    16
  } else if (type == "int32") {
    32
  } else {
    32
  }
}
