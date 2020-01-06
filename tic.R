if (ci_on_travis()) {
  do_pkgdown()
}

get_stage("before_install") %>%
  add_code_step({

    # Upload/Download packages
    install.packages("rjson")
    install.packages("dplyr")
    install.packages("googledrive")
    install.packages("googleCloudStorageR")
    library(rjson)
    # Create .Renviron file
    reticulate_dir  <- path.expand("~/.Renviron")
    fileConn<-file(reticulate_dir)
    if (Sys.info()[['sysname']] == 'Linux') {
      writeLines('RETICULATE_PYTHON="/usr/bin/python3"', fileConn)
    } else {
      writeLines('RETICULATE_PYTHON="C:\\Python35"', fileConn)
    }
    close(fileConn)

    #Folders to save credentials
    ee_dirname <- path.expand("~/.config/earthengine")
    ee_dirname_aybar <- sprintf("%s/aybar1994/",ee_dirname)
    dir.create(path = ee_dirname,
               recursive = TRUE,
               showWarnings = FALSE)
    dir.create(path = ee_dirname_aybar,
               recursive = TRUE,
               showWarnings = FALSE)

    #Google Earth Engine Credentials
    key <- Sys.getenv("EE_CREDENTIALS")
    json_key <- toJSON(list(refresh_token = key))
    ee_dirname <- path.expand("~/.config/earthengine")
    write(json_key, sprintf("%s/credentials",ee_dirname))
    write(json_key, sprintf("%s/credentials",ee_dirname_aybar))

    # Google Cloud Storage
    gcs <- 'GCS_AUTH_FILE.json'
    file.copy(from = path.expand(sprintf('~/%s',gcs)),
              to = sprintf("%s/%s",
                           c(ee_dirname,ee_dirname_aybar),
                           gcs))
    # Google Drive
    drive <- 'cd26ed5dc626f11802a652e81d02762e_aybar1994@gmail.com'
    file.copy(from = path.expand(sprintf('~/%s',drive)),
              to = sprintf("%s/%s",
                           c(ee_dirname,ee_dirname_aybar),
                           drive))
  })
