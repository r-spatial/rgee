do_package_checks()

if (ci_on_travis()) {
  do_pkgdown()
}

get_stage("before_install") %>%
  add_code_step({
    system("openssl version")
    install.packages("rjson")
    install.packages("googledrive")
    install.packages("googleCloudStorageR")

    library(googledrive)
    library(googleCloudStorageR)
    library(rjson)

    reticulate_dir  <- path.expand("~/.Renviron")
    fileConn<-file(reticulate_dir)
    writeLines('RETICULATE_PYTHON="/usr/bin/python3"', fileConn)
    close(fileConn)

    #GOOGLE EARTH ENGINE
    key <- Sys.getenv("EE_CREDENTIALS")
    json_key <- toJSON(list(refresh_token = key))
    ee_dirname <- path.expand("~/.config/earthengine")
    dir.create(ee_dirname, recursive = TRUE,showWarnings = FALSE)
    dir.create(paste0(ee_dirname,'/aybar1994/'), recursive = TRUE,showWarnings = FALSE)
    write(json_key, sprintf("%s/credentials",ee_dirname))

    #GOOGLE DRIVE & CLOUD STORAGE
    # url_dir <- "https://github.com/csaybar/rgee/raw/master/tests/credentials/"
    # output_dir <- sprintf("%s/aybar1994/",ee_dirname)
    # dir.create(output_dir, recursive = TRUE,showWarnings = FALSE)

    #DRIVE
    # googledrive_dirname <- path.expand("~/.R/gargle/gargle-oauth/")
    # dir.create(googledrive_dirname, recursive = TRUE,showWarnings = FALSE)
    # file_01 <- sprintf("%s%s", url_dir, "cd26ed5dc626f11802a652e81d02762e_aybar1994@gmail.com.enc")
    # loca_file_01 <- paste0(googledrive_dirname,basename(file_01))
    # download.file(file_01,loca_file_01)
    # desencryp_01 <- sprintf("openssl aes-256-cbc -K $encrypted_bba51a8df4d6_key -iv $encrypted_bba51a8df4d6_iv -in %s -out %s -d",
    #                         loca_file_01,
    #                         gsub("\\.enc$","",loca_file_01))
    #GCS
    # file_02 <- sprintf("%s%s", url_dir, "GCS_AUTH_FILE.json.enc")
    # loca_file_02 <- paste0(output_dir,basename(file_02))
    # download.file(file_02,loca_file_02)
    # desencryp_02 <- sprintf("openssl aes-256-cbc -K $encrypted_bba51a8df4d6_key -iv $encrypted_bba51a8df4d6_iv -in %s -out %s -d",
    #                         loca_file_02,
    #                         gsub("\\.enc$","",loca_file_02))
    # print(desencryp_01)
    # print(desencryp_02)
    # system(desencryp_01)
    # system(desencryp_02)
  })
