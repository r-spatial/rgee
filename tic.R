do_package_checks()

if (ci_on_travis()) {
  do_pkgdown()
}

get_stage("install") %>%
  add_step(step_install_cran("magick"))

travis::travis_get_builds()
run_all_stages()
