#' Return documentation of Earth Engine modules, methods and classes
#' @noRd
ee_help_addins <- function(debug=FALSE, content) {
  if (!debug) {
    selected_content <- content
  } else {
    context <- rstudioapi::getSourceEditorContext()
    selected_content <- context$selection[[1]]$text
  }
  # If press Ctrl + Enter
  if (selected_content == "") {
    try(ee_help(ee_get_eefunc()), silent = TRUE)
    # If first select the text and after that Ctrl + Enter
  } else {
    selected_content_filtered <- gsub("\n|[[:space:]]","", selected_content)
    try(ee_help(selected_content_filtered), silent = TRUE)
  }
  invisible(TRUE)
}

#' How many white space we deleted?
#' @noRd
ee_space_removed <- function(text, cursor) {
  text <- strsplit(text,"")[[1]]
  sum(grepl(" ",text[1:cursor]))
}

#' Merge forward and backward
#' @noRd
ee_get_funname <- function(text, cursor) {
  text <- strsplit(text,"")[[1]]

  if (length(text) < cursor) {
    return(invisible(FALSE))
  }

  if (text[cursor] == "(") {
    cursor <- cursor -1
  }
  if (cursor == 1) {
    # the last word can not be a $
    paste0(text[1:forward(text, cursor)], collapse = "")
  } else {
    # the last word can not be a $
    paste0(text[backward(text, cursor):forward(text, cursor)], collapse = "")
  }
}

#' Search words forward
#' @noRd
forward <- function(x, cursor) {
  forward_range <- cursor:length(x)
  for (index in forward_range) {
    is_letter <- grepl("[a-zA-Z_]", x[index])
    if (!is_letter) {
      index <- index - 1
      break
    }
  }
  if (is_letter <- grepl("\\$", x[index])) {
    index - 1
  } else {
    index
  }
}

#' Search words backward
#' @noRd
backward <- function(x, cursor) {
  index <- cursor
  repeat {
    if (index == 1) {
      break
    }
    # Just pass the letter if is inside a ()
    if (x[index] == ")") {
      count_par <- 1
      counter <- 0
      while (count_par != 0) {
        if (x[index] == "(") {
          count_par <- count_par - 1
        } else if(x[index] == ")" & counter != 0) {
          count_par <- count_par + 1
        }
        index <- index - 1
        counter <- counter + 1
        if (index == 1) {
          break
        }
        # print(sprintf("%s:%s",counter,count_par))
      }
      index <- index - 1
    }

    if (grepl("[a-zA-Z_]|\\$|\\)", x[index])) {
      index <- index - 1
    } else {
      index <- index + 1
      break
    }
  }
  index
}

#' Aux function useful to know if a multiline (recursive)
#' Returns a logical vector.
#' @noRd
is_multilines_r <- function(context, line) {
  if (is_multilines(context, line)) {
    c(TRUE, is_multilines_r(context, line - 1))
  } else {
    FALSE
  }
}

#' Aux function useful to know if a multiline
#' Returns a logical value.
#' @noRd
is_multilines <- function(context, line) {
  if (line == 1) {
    FALSE
  } else {
    line_of_code_1 <- context$contents[line]
    text_1 <- strsplit(line_of_code_1, "")[[1]]
    is_white_space <- text_1[1] == " "

    line_of_code_2 <- context$contents[line - 1]
    text_2 <- strsplit(line_of_code_2, "")[[1]]
    is_dolar <- text_2[length(text_2)] == "$"
    if (length(is_dolar ) == 0) {
      is_dolar <- FALSE
    }

    if (is_dolar & is_white_space) {
      TRUE
    } else {
      FALSE
    }
  }
}

#' Returns the EE function name
#' @noRd
ee_get_eefunc <- function() {
  # get rstudio context
  context <- rstudioapi::getSourceEditorContext()
  cursor <- context$selection[[1]]$range[[1]][2]
  line <- context$selection[[1]]$range[[1]][1]

  # is a multiple line?
  if (any(is_multilines_r(context, line))) {
    # lines above!
    number_of_extra_lineas <- sum(is_multilines_r(context, line))
    lines <- (line - number_of_extra_lineas):line
    # merge lines text in one character
    text_merge <- paste0(gsub(" ", "", context$contents[lines]), collapse = "")
    # upgrade cursor
    extra_lines <- lines[-length(lines)]
    previous_len <- paste0(context$contents[extra_lines], collapse = "")
    space_removed <- ee_space_removed(text = context$contents[line], cursor =  cursor)
    new_cursor <- nchar(previous_len) + cursor - space_removed
    ee_get_funname(text = text_merge, cursor =  new_cursor)
  } else {
    ee_get_funname(text = context$contents[line], cursor =  cursor)
  }
}

