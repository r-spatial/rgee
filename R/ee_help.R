#' Documentation for Earth Engine Objects
#' @param eeobject Earth Engine Object to print documentation.
#' @param browser Logical. Display documentation in the browser.
#' @importFrom reticulate py_function_docs
#' @importFrom utils tail
#' @family helper functions
#' @examples
#' \dontrun{
#' library(rgee)
#' ee_Initialize()
#'
#' ee$Image()$geometry()$centroid %>% ee_help()
#' ee$Image()$geometry() %>% ee_help()
#' ee$Geometry$Rectangle(c(-110.8, 44.6, -110.6, 44.7)) %>% ee_help()
#' ee$Image %>% ee_help()
#' ee$Image %>% ee_help(browser = TRUE)
#' }
#' @export
ee_help <- function(eeobject, browser = FALSE) {
  #obs : simple earth engine objects like ee$Number will return NULL
  eequery_scope <- try(
    expr = unlist(jsonlite::parse_json(eeobject$serialize())$scope),
    silent = TRUE
  )
  # If eeobject is an Earth Engine object get the last function
  if (class(eequery_scope) != 'try-error' & !is.null(eequery_scope)) {
    search_funnames <- grepl("functionName", names(eequery_scope))
    ee_functions <- eequery_scope[search_funnames]
    fun_name <- paste0("ee$",gsub("\\.","$",tail(ee_functions,1)))
  } else {
    if (is.character(eeobject)) {
      fun_name <- eeobject
      # fun_name has parenthesis
      fun_name_d <- strsplit(fun_name, "\\$")[[1]]
      exist_parenthesis <- grepl(
        pattern = "(",
        x = fun_name_d[length(fun_name_d)],
        fixed = TRUE
      )
    } else {
      exist_parenthesis <- FALSE
      wrap_lhs <- function(x) gsub("rgee", "", ee_get_lhs())
      fun_name <- wrap_lhs(eeobject)
      if (length(fun_name) == 0) {
        fun_name <- deparse(substitute(eeobject))
      }
    }

    if (is.null(eequery_scope) | exist_parenthesis) {
      components <- strsplit(fun_name, "\\$")[[1]]
      topic <- components[[length(components)]]
      source <- paste(components[1:(length(components) - 1)],
                      collapse = "$")
      # The name is a base function?
      is_a_basefunction <- tryCatch(
        expr = {eval(parse(text = sprintf("base::%s", fun_name))); TRUE},
        error = function(e) FALSE
      )
      if (isTRUE(is_a_basefunction)) {
        stop(
          "'", fun_name, "' is not subsettable. Are you using a ",
          "function name that matches the names of the R base",
          " library?. If 'base::", fun_name, "' exists ee_help will not work."
        )
      }
      if (topic == source) {
        fun_name <- topic
      } else {
        # Remove just the last parenthesis
        extract_parenthesis_text <- gregexpr("(?=\\().*?(?<=\\))",
                                             topic,
                                             perl = TRUE)
        parenthesis_text <- regmatches(topic, extract_parenthesis_text)[[1]]
        to_display <- gsub(parenthesis_text, "", topic, fixed = TRUE)
        to_display <- gsub("\\(|\\)", "", to_display)
        fun_name <- paste(source,to_display,sep = "$")
      }
    }
  }

  if (fun_name == "ee") {
    doc_to_display <- ee_module_help()
  } else {
    doc_to_display <- tryCatch(
      expr = fun_name %>%
        paste(collapse = '') %>%
        ee_function_docs,
      error = function(e) ee_real_name(fun_name) %>%
        paste(collapse = '') %>%
        ee_function_docs
    )
  }

  # Creating html to display
  temp_file <- sprintf("%s/ee_help.html", tempdir())
  file.create(temp_file)
  fileConn <- file(temp_file)

  # Are you in Rstudio?
  if (.Platform$GUI == "RStudio" & isFALSE(browser)) {
    if (!requireNamespace("rstudioapi", quietly = TRUE)) {
      stop("package rstudioapi required, please install it first")
    }
    writeLines(
      text = c(
        ee_html_head_rstudio(doc_to_display$qualified_name),
        ee_html_title_rstudio(doc_to_display$title),
        ee_html_description_rstudio(doc_to_display$description),
        ee_html_usage_rstudio(doc_to_display),
        ee_html_arguments_rstudio(doc_to_display$parameters),
        ee_html_details_rstudio(doc_to_display$details),
        ee_html_returns_rstudio(doc_to_display$returns)
      ),
      con = fileConn
    )
    close(fileConn)
    rstudioapi::viewer(temp_file)
  } else {
    writeLines(
      text = c(
        ee_html_head_simple(doc_to_display$qualified_name),
        ee_html_title_simple(doc_to_display$description),
        ee_html_description_simple(doc_to_display$description),
        ee_html_usage_simple(doc_to_display),
        ee_html_arguments_simple(doc_to_display$parameters),
        ee_html_details_simple(doc_to_display$details),
        ee_html_returns_simple(doc_to_display$returns)
      ),
      con = fileConn
    )
    close(fileConn)
    browseURL(temp_file)
  }
  invisible(temp_file)
}


#' Create init table - R Documentation Simple
#' @noRd
ee_html_head_simple <- function(fun_name) {
  '<table width="100%" summary="page for fun_rgee {rgee}">
     <tbody>
     <tr>
       <td align="left"> fun_rgee {rgee}</td>
       <td align="right">R Documentation</td>
     </tr>
     </tbody>
   </table>' -> message
  message <- gsub("
", "", message)
  gsub("fun_rgee", fun_name, message)
}

#' Create init table - R Documentation Rstudio
#' @noRd
ee_html_head_rstudio <- function(fun_name) {
  td_style <- "font-family: sans-serif;font-size: 10pt;"
  '<table width="100%" summary="page for fun_rgee {rgee}">
     <tbody>
     <tr>
       <td align="left" style="td_style"> fun_rgee {rgee} </td>
       <td align="right" style="td_style"> R Documentation </td>
     </tr>
     </tbody>
   </table>' -> message
  message <- gsub("
", "", message)
  message_norgee <- gsub("fun_rgee", fun_name, message)
  gsub("td_style", td_style, message_norgee)
}


#' Create main title - R Documentation Simple
#' @noRd
ee_html_title_simple <- function(title) {
  sprintf('<h2 style="%s">%s</h2>', ee_css_h2_simple(), title)
}

#' Create main title - R Documentation Rstudio
#' @noRd
ee_html_title_rstudio <- function(title) {
  sprintf('<h2 style="%s">%s</h2>', ee_css_h2_rstudio(), title)
}

#' Create description - R Documentation Simple
#' @noRd
ee_html_description_simple <- function(descrp) {
  api_ref <- "https://developers.google.com/earth-engine/api_docs"
  gee_message <- sprintf(
    ' Documentation obtained from this <a href="%s">link</a>.',
    api_ref
  )
  sprintf(
    '<h3 style="%s">Description</h3>\n\n<p>%s%s<p>',
    ee_css_h3_simple(),
    descrp,
    gee_message
  )
}

#' Create description - R Documentation Simple
#' @noRd
ee_html_description_rstudio <- function(descrp) {
  api_ref <- "https://developers.google.com/earth-engine/api_docs"
  gee_message <- sprintf(
    ' Documentation obtained from this <a href="%s">link</a>.',
    api_ref
  )
  p_style <- "font-family: sans-serif; font-size: 10pt;"
  sprintf(
    '<h3 style="%s">Description</h3><p style="%s">%s%s<p>',
    ee_css_h3_rstudio(),
    p_style,
    descrp,
    gee_message
  )
}

#' Create usage - R Documentation Simple
#' @noRd
ee_html_usage_simple <- function(pydoc) {
  name_strong <- sprintf("<strong>%s</strong>", pydoc$qualified_name)
  if (is.null(pydoc$signature)) {
    return("")
  }
  extract_parenthesis_text <- gregexpr("(?=\\().*?(?<=\\))",
    pydoc$signature,
    perl = TRUE
  )
  to_display <- name_strong %>%
    paste0(regmatches(pydoc$signature, extract_parenthesis_text)[[1]])
  sprintf(
    '<h3 style="%s">Usage</h3><p style = "%s">%s</p>',
    ee_css_h3_simple(),
    ee_css_pre_simple(),
    to_display
  )
}

#' Create usage - R Documentation Simple
#' @noRd
ee_html_usage_rstudio <- function(pydoc) {
  name_strong <- sprintf("<strong>%s</strong>", pydoc$qualified_name)
  if (is.null(pydoc$signature)) {
    return("")
  }
  extract_parenthesis_text <- gregexpr("(?=\\().*?(?<=\\))",
    pydoc$signature,
    perl = TRUE
  )
  to_display <- name_strong %>%
    paste0(regmatches(pydoc$signature, extract_parenthesis_text)[[1]])
  sprintf('<h3 style="%s">Usage</h3><p style = "%s">%s</p>',
          ee_css_h3_rstudio(),
          ee_css_pre_simple(),
          to_display)
}

#' Create arguments - R Documentation Simple
#' @noRd
ee_html_arguments_simple <- function(parameters) {
  if (is.null(parameters)) {
    return("")
  }
  if (length(parameters) == 0) {
    return("")
  }
  p_style <- paste0("display: block;margin-block-start: 1em;",
                    "margin-block-end: 1em;margin-inline-start: 0px;",
                    "margin-inline-end: 0px;")
  table_style <- paste0("display: table;border-collapse: separate;",
                        "border-spacing: 2px;border-color: grey;")
  td_style <- "display: table-cell;vertical-align: inherit;"
  table_head <- sprintf('<table summary="R argblock" style="%s">', table_style)
  table_end <- "</table>"
  rows <- ""
  for (index in seq_along(parameters)) {
    parameter <- parameters[index]
    param_name <- names(parameter)
    row_message <- paste0(
      sprintf('<tr valign="top"><td style="%s"><code>%s</code></td>',
              td_style,
              param_name),
      sprintf('<td style="%s"><p style="%s">%s</p></td></tr>',
              td_style,
              p_style,
              parameter)
    )
    rows <- paste0(rows, row_message)
  }
  arguments_table <- paste0(table_head, rows, table_end)
  sprintf('<h3 style="%s">Arguments</h3>%s',
          ee_css_h3_simple(),
          arguments_table)
}

#' Create arguments - R Documentation Simple
#' @noRd
ee_html_arguments_rstudio <- function(parameters) {
  if (is.null(parameters)) {
    return("")
  }
  if (length(parameters) == 0) {
    return("")
  }
  p_style <- paste0("display: block;margin-block-start: 1em;",
                    "margin-block-end: 1em;margin-inline-start: 0px;",
                    "margin-inline-end: 0px;")
  table_style <- paste0("display: table;border-collapse: separate;",
                        "border-spacing: 10px;border-color: grey;")
  td_style <- paste0("display: table-cell;vertical-align: inherit;",
                     "font-family: sans-serif;font-size: 10pt;")
  table_head <- sprintf('<table summary="R argblock" style="%s">', table_style)
  table_end <- "</table>"
  rows <- ""
  for (index in seq_along(parameters)) {
    parameter <- parameters[index]
    param_name <- names(parameter)
    row_message <- paste0(
      sprintf('<tr valign="top"><td style="%s"><code>%s</code></td>',
              td_style,
              param_name),
      sprintf('<td style="%s"><p style="%s">%s</p></td></tr>', td_style,
              p_style,
              parameter)
    )
    rows <- paste0(rows, row_message)
  }
  arguments_table <- paste0(table_head, rows, table_end)
  sprintf('<h3 style="%s">Arguments</h3>%s',
          ee_css_h3_rstudio(),
          arguments_table)
}

#' Create details - R Documentation Simple
#' @noRd
ee_html_details_simple <- function(details) {
  if (is.null(details)) {
    return(details)
  }
  if (nchar(details) < 1) {
    details
  } else {
    sprintf('<h3 style="%s">Details</h3><p>%s<p>',
            ee_css_h3_simple(),
            details)
  }
}

#' Create details - R Documentation Rstudio
#' @noRd
ee_html_details_rstudio <- function(details) {
  if (is.null(details)) {
    return(details)
  }
  if (nchar(details) < 1) {
    details
  } else {
    p_style <- "font-family: sans-serif; font-size: 10pt;"
    sprintf('<h3 style="%s">Details</h3><p style="%s">%s<p>',
            ee_css_h3_rstudio(),
            p_style,
            details)
  }
}

#' Create returns - R Documentation Simple
#' @noRd
ee_html_returns_simple <- function(returns) {
  if (is.null(returns)) {
    return(returns)
  }
  if (nchar(returns) < 1) {
    returns
  } else {
    sprintf('<h3 style="%s">Returns</h3><p>%s<p>',
            ee_css_h3_simple(),
            returns)
  }
}


#' Create returns - R Documentation Simple
#' @noRd
ee_html_returns_rstudio <- function(returns) {
  if (is.null(returns)) {
    return(returns)
  }
  if (nchar(returns) < 1) {
    returns
  } else {
    p_style <- "font-family: sans-serif; font-size: 10pt;"
    sprintf('<h3 style="%s">Returns</h3><p style="%s">%s<p>',
            ee_css_h3_rstudio(),
            p_style,
            returns)
  }
}

#' pre tag CSS-style - Simple
#' @noRd
ee_css_pre_simple <- function() {
  "font-family: monospace;"
}

#' h2 tag CSS-style - Simple
#' @noRd
ee_css_h2_simple <- function() {
  paste0(
    "background: white;color: rgb(40%, 40%, 40%);font-family: monospace",
    ";font-size: large;text-align: center;"
  )
}

#' h2 tag CSS-style - Rstudio
#' @noRd
ee_css_h2_rstudio <- function() {
  "font-size: x-large;font-weight: normal;font-family: sans-serif;"
}

#' h3 tag CSS-style - Simple
#' @noRd
ee_css_h3_simple <- function() {
  paste0(
    "background: white;color: rgb(40%, 40%, 40%);font-family: monospace",
    ";font-size: large;"
  )
}

#' h3 tag CSS-style - Rstudio
#' @noRd
ee_css_h3_rstudio <- function() {
  paste0(
    "background: white;color: rgb(35%, 35%, 35%);font-family: sans-serif",
    ";font-size: 15px;"
  )
}

#' Get lhs argument from a pipe
#' @noRd
ee_get_lhs <- function() {
  parents <- lapply(sys.frames(), parent.env)
  is_magrittr_env <-
    vapply(parents, identical, logical(1), y = environment(`%>%`))
  if (any(is_magrittr_env)) {
    deparse(get("lhs", sys.frames()[[max(which(is_magrittr_env))]]))
  }
}

#' Scaffold R wrappers for Python functions
#'
#' @param python_function Fully qualified name of Python function or class
#' constructor (e.g. ee$Image()$geometry()$Rectangle)
#' @noRd
ee_function_docs <- function(ee_function) {
  inspect <- import("inspect")
  function_docs <- inspect$getdoc(eval(parse(text = ee_function)))
  output_help <- py_function_docs(ee_function)
  real_description <- paste(output_help$description,output_help$details)
  real_args <- ee_help_create_arg(function_docs)
  output_help$title <- output_help$description
  output_help$description <- gsub("\n"," ",real_description)
  output_help$details <- ""
  output_help$parameters <- real_args$arg
  output_help$signature <- sprintf(
    "%s(%s, ...)",
    gsub( " *\\(.*?\\) *", "", output_help$signature),
    real_args$signature)
  output_help
}

#' Get the real name of the function
#' @noRd
ee_real_name <- function(ee_function){
  components <- strsplit(ee_function, "\\$")[[1]]
  topic <- components[[length(components)]]
  source <- paste(components[1:(length(components) - 1)],
                  collapse = "$")
  fn_name <- paste0(source,"$name()")
  ee_object_name <- tryCatch(
    expr = eval(parse(text = fn_name)),
    error = function(e) stop(
      "ee_help was not able to determinate the function name."
    )
  )
  sprintf("ee$%s$%s",ee_object_name,topic)
}


#' Create args argument
#' @noRd
ee_help_create_arg <- function(function_docs) {
  # get just the argument text
  arguments <- strsplit(function_docs,"(\nArgs:\n) ")[[1]][2]
  if (is.na(arguments)) {
    return(list(signature = "cls", arg = ""))
  }
  arguments <- gsub("Returns.*","", arguments)
  groups <- strsplit(arguments,"\n")[[1]]
  group_condition <- grepl("^\\s*[aA0-zZ9|**]*:", groups, perl = TRUE)

  #Create text groups
  walk <- 0
  result <- rep(NA, length(group_condition))
  for (index in seq_along(group_condition)) {
    cond <- group_condition[index]
    walk <- walk + cond
    result[index] <- walk
  }
  # Handling text inside the groups
  arguments_des <- rep(NA, length(unique(result)))
  arguments_name <- rep(NA, length(unique(result)))

  for (group in unique(result)) {
    message <- paste0(groups[which(result == group)],collapse = "")
    arg <- sub("^([^:]+:).+$", "\\1", message)
    arg_clean  <-  trimws(sub(":","",sub("\\s","",arg)))
    message_clean <- trimws(gsub("\\s+"," ", sub(arg,"",message, fixed = TRUE)))
    arguments_des[group] <- message_clean
    arguments_name[group] <- arg_clean
  }
  names(arguments_des) <- arguments_name
  arguments_des <- arguments_des[!names(arguments_des) %in% "DEPRECATED"]
  arguments_name <- arguments_name[!arguments_name == "DEPRECATED"]
  signature_text <- paste(arguments_name, collapse = ", ")
  return(list(arg = arguments_des, signature = signature_text))
}


#' ee module help
#' @noRd
ee_module_help <- function() {
  list(
    name = "",
    qualified_name = "ee",
    description = "Interface to main Earth Engine module. Provides access to top level classes and functions as well as sub-modules (e.g. ee$Image, ee$FeatureCollection$first, etc.).",
    details = "",
    signature = "ee",
    parameters = "",
    sections = list(),
    returns = NULL,
    title = "Main Earth Engine module"
  )
}
