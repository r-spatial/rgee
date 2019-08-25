do_package_checks()

if (ci_on_travis()) {
  do_pkgdown()
}

get_stage("script") %>%
  add_code_step({
    install.packages("rjson")
    library(rjson)
    key <- Sys.getenv("EE_CREDENTIALS")
    json_key <- toJSON(list(refresh_token = key))
    ee_dirname <- path.expand("~/.config/earthengine")
    dir.create(ee_dirname)
    write(json_key, sprintf("%s/credentials",ee_dirname))
  })
