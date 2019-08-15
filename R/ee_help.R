# Para clases
ee_help <- function(module){
  if(!require(rstudioapi)) install.packages("rstudioapi")
  library(rstudioapi)

  tmp <- reticulate:::py_capture_output(import_builtins()$help(ee$Geometry), type = "stdout")
  html_template <- paste(readLines("https://raw.githubusercontent.com/ryali93/rgee/master/inst/index.html"), collapse = " ")
  tmp_str <- strsplit(tmp, "\n")[[1]]
  textoHtml = ""
  for(n in 1:length(tmp_str)){
    linea <- gsub(x = c(tmp_str[n]), pattern = "|", replacement = "", fixed = T)
    lineHtml <- paste("<p>", linea, "</p>")
    if(grepl(":", linea)){
      lineHtml <- paste("<h4>", linea, "</h4>")
    }
    if(grepl("class", linea) & grepl(":", linea)){
      lineHtml <- paste("<h2>", linea, "</h2>")
    }
    if(grepl("Method", linea) & grepl(":", linea)){
      lineHtml <- paste("<h3>", linea, "</h3>")
    }
    if(grepl("      ", linea)){
      lineHtml <- paste("<p>", linea, "</p>")
    }
    textoHtml <- paste(textoHtml, lineHtml)
  }
  html_doc <- gsub(x = c(html_template), pattern = "LoremIpsum", replacement = textoHtml)

  dir <- tempfile()
  dir.create(dir)
  htmlFile <- file.path(dir, "index.html")
  write.table(html_doc, htmlFile)
  rstudioapi::viewer(htmlFile)
}

#ee_help(ee$Geometry)







