#' Evaluate ee query
#'
#' @param  ... expr
#'
#' @export
ee_evaluate <- function(...) {
  oauth_func_path <- system.file("Python/ee_auth.py", package = "rgee")
  ee_source_python(oauth_func_path)
  total_name <- deparse(substitute(...))
  # TODO -> improve regex
  total_name %>%
    paste(., collapse = "") %>%
    gsub("\\ee_", "\\ee.", .) %>%
    gsub("%>%", "\\.", .) %>%
    gsub("\\s", "", .) %>%
    py_evaluate(.)
}


#' Return ee query
#'
#' @param  ... expr
#' @export
ee_query <- function(...) {
  oauth_func_path <- system.file("Python/ee_auth.py", package = "rgee")
  ee_source_python(oauth_func_path)
  total_name <- deparse(substitute(...))
  # TODO -> improve regex
  total_name %>%
    paste(., collapse = "") %>%
    gsub("%>%", "\\.", .) %>%
    gsub("\\s", "", .)
}
