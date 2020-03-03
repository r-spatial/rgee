#' Documentation for Earth Engine Objects
#' @param eeobject 	Earth Engine Object to print documentation.
#' @importFrom reticulate py_function_docs
#' @examples
#' \dontrun{
#' ee_Initialize()
#' ee_help(eeobject = ee$Image()$geometry()$centroid)
#' ee_help(eeobject = ee$Image()$geometry)
#' ee_help(eeobject = ee$ImageCollection(list())$size)
#' }
ee_help <- function(eeobject) {
  wrap_lhs <- function(x) gsub(x,"",ee_get_lhs())
  fun_name <- wrap_lhs(eeobject)
  if (length(fun_name) == 0) {
    fun_name <- deparse(substitute(eeobject))
  }
  doc_to_display <- py_function_docs(fun_name)

  # Creating html to display
  temp_file <- sprintf("%s/ee_help.html", tempdir())
  file.create(temp_file)
  fileConn <- file(temp_file)

  # Are you in Rstudio?
  if (rstudioapi::isAvailable()) {
    writeLines(
      text = c(ee_html_head_rstudio(doc_to_display$qualified_name),
               ee_html_title_simple(doc_to_display$description),
               ee_html_description_simple(doc_to_display$description),
               ee_html_usage_simple(doc_to_display),
               ee_html_arguments_simple(doc_to_display$parameters),
               ee_html_details_simple(doc_to_display$details),
               ee_html_returns_simple(doc_to_display$returns)),
      con =  fileConn
    )
    close(fileConn)
    rstudioapi::viewer(temp_file)
  } else {
    writeLines(
      text = c(ee_html_head_simple(doc_to_display$qualified_name),
               ee_html_title_simple(doc_to_display$description),
               ee_html_description_simple(doc_to_display$description),
               ee_html_usage_simple(doc_to_display),
               ee_html_arguments_simple(doc_to_display$parameters),
               ee_html_details_simple(doc_to_display$details),
               ee_html_returns_simple(doc_to_display$returns)),
      con =  fileConn
    )
    close(fileConn)
    browseURL(temp_file)
  }
}


#' Create init table - R Documentation Simple
#' @noRd
ee_html_head_simple <- function(fun_name){
  '<table width="100%" summary="page for fun_rgee {rgee}">
     <tbody>
     <tr>
       <td align="left"> fun_rgee {rgee}</td>
       <td align="right">R Documentation</td>
     </tr>
     </tbody>
   </table>' -> message
  message <- gsub('\n', '', message)
  gsub('fun_rgee', fun_name, message)
}

#' Create init table - R Documentation Rstudio
#' @noRd
ee_html_head_rstudio <- function(fun_name) {
  td_style <- 'font-family: sans-serif;font-size: 10pt;'
  '<table width="100%" summary="page for fun_rgee {rgee}">
     <tbody>
     <tr>
       <td align="left" style="td_style"> fun_rgee {rgee} </td>
       <td align="right" style="td_style"> R Documentation </td>
     </tr>
     </tbody>
   </table>' -> message
  message <- gsub('\n', '', message)
  message_norgee <- gsub('fun_rgee', fun_name, message)
  gsub('td_style', td_style, message_norgee)
}

#' Create main title - R Documentation Simple
#' @noRd
ee_html_title_simple <- function(title){
  sprintf('<h2 style="%s">%s</h2>',ee_css_h2_rstudio(),title)
}

#' Create description - R Documentation Simple
#' @noRd
ee_html_description_simple <- function(descrp){
  api_ref <- 'https://developers.google.com/earth-engine/api_docs'
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
ee_html_description_rstudio <- function(descrp){
  api_ref <- 'https://developers.google.com/earth-engine/api_docs'
  gee_message <- sprintf(
    ' Documentation obtained from this <a href="%s">link</a>.',
    api_ref
  )
  sprintf(
    '<h3 style="%s">Description</h3>\n\n<p>%s%s<p>',
    ee_css_h3_rstudio(),
    descrp,
    gee_message
  )
}

#' Create usage - R Documentation Simple
#' @noRd
ee_html_usage_simple <- function(pydoc){
  name_strong <- sprintf("<strong>%s</strong>",pydoc$qualified_name)
  if (is.null(pydoc$signature)) {
    return("")
  }
  extract_parenthesis_text <- gregexpr("(?=\\().*?(?<=\\))", pydoc$signature, perl = TRUE)
  to_display <- name_strong %>%
    paste0(regmatches(pydoc$signature, extract_parenthesis_text)[[1]])
  sprintf('<h3 style="%s">Usage</h3><p style = "%s">%s</p>',ee_css_h3_simple(),ee_css_pre_simple(),to_display)
}

#' Create arguments - R Documentation Simple
#' @noRd
ee_html_arguments_simple <- function(parameters) {
  if (is.null(parameters)) {
    return("")
  }
  p_style <- 'display: block;margin-block-start: 1em;margin-block-end: 1em;margin-inline-start: 0px;margin-inline-end: 0px;'
  table_style <- 'display: table;border-collapse: separate;border-spacing: 2px;border-color: grey;'
  td_style <- 'display: table-cell;vertical-align: inherit;'
  table_head <- sprintf('<table summary="R argblock" style="%s">',table_style)
  table_end <- '</table>'
  rows <- ""
  for (index in seq_along(parameters)) {
    parameter <- parameters[index]
    param_name <- names(parameter)
    row_message <- paste0(
      sprintf('<tr valign="top"><td style="%s"><code>%s</code></td>',td_style, param_name),
      sprintf('<td style="%s"><p style="%s">%s</p></td></tr>', td_style,p_style, parameter)
    )
    rows <- paste0(rows,row_message)
  }
  arguments_table <- paste0(table_head,rows,table_end)
  sprintf('<h3 style="%s">Arguments</h3>%s',ee_css_h3_simple(), arguments_table)
}


#' Create details - R Documentation Simple
#' @noRd
ee_html_details_simple <- function(details){
  if (is.null(details)) return(details)
  if (nchar(details) < 1) {
    details
  } else {
    sprintf('<h3 style="%s">Details</h3><p>%s<p>',ee_css_h3_simple(), details)
  }
}

#' Create returns - R Documentation Simple
#' @noRd
ee_html_returns_simple <- function(returns) {
  if (is.null(returns)) return(returns)
  if (nchar(returns) < 1 ) {
    returns
  } else {
    sprintf('<h3 style="%s">Returns</h3><p>%s<p>',ee_css_h3_simple(), returns)
  }
}

#' pre tag CSS-style - Simple
#' @noRd
ee_css_pre_simple <- function(){
  'font-family: monospace;'
}

#' h2 tag CSS-style - Simple
#' @noRd
ee_css_h2_simple <- function(){
  paste0('background: white;color: rgb(40%, 40%, 40%);font-family: monospace',
         ';font-size: large;text-align: center;')
}

#' h2 tag CSS-style - Rstudio
#' @noRd
ee_css_h2_rstudio <- function(){
  'font-size: x-large;font-weight: normal;font-family: sans-serif;'
}

#' h3 tag CSS-style - Simple
#' @noRd
ee_css_h3_simple <- function(){
  paste0('background: white;color: rgb(40%, 40%, 40%);font-family: monospace',
         ';font-size: large;')
}

#' h3 tag CSS-style - Rstudio
#' @noRd
ee_css_h3_rstudio <- function(){
  paste0('color: rgb(35%, 35%, 35%);font-family: sans-serif',
         ';font-size: 1.17em;font-weight: bold;')
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
