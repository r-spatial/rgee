ee_source_python <- function(oauth_func_path) {
  module_name <- gsub("\\.py$", "", basename(oauth_func_path))
  module_path <- dirname(oauth_func_path)
  reticulate::import_from_path(module_name, path = module_path, convert = FALSE)
}

git <- function(..., echo_cmd = TRUE, echo = TRUE, error_on_status = TRUE) {
  processx::run("git", c(...),
    echo_cmd = echo_cmd, echo = echo,
    error_on_status = error_on_status
  )
}

git_has_remote_branch <- function(remote, branch) {
  has_remote_branch <- git(
    "ls-remote",
    "--quiet",
    "--exit-code",
    remote, branch,
    echo = FALSE, echo_cmd = FALSE, error_on_status = FALSE
  )$status == 0
}

old_branch <- function() {
  branch <- git("rev-parse", "--abbrev-ref", "HEAD",
    echo = FALSE,
    echo_cmd = FALSE
  )$stdout
  sub("\n$", "", branch)
}

ci_commit_sha <- function() {
  env_vars <- c("TRAVIS_COMMIT", "GITHUB_SHA")
  for (var in env_vars) {
    commit_sha <- Sys.getenv(var, "")
    if (commit_sha != "") {
      return(commit_sha)
    }
  }
  ""
}

construct_commit_message <- function(pkg, commit = ci_commit_sha()) {
  rgee_version <- packageVersion("rgee")
  sprintf(
    "Update rgee: %s@%s", rgee_version,
    substr(commit, 1, 7)
  )
}

github_worktree_add <- function(dir, remote, branch) {
  cli::rule("Adding worktree", line = 1)
  git("worktree", "add", dir, paste0(remote, "/", branch))
}

github_worktree_remove <- function(dir) {
  cli::rule("Removing worktree", line = 1)
  git("worktree", "remove", dir)
}

with_dir <- function(new, code) {
  old <- setwd(dir = new)
  on.exit(setwd(old))
  force(code)
}

github_push <- function(dest_dir, commit_message, remote, branch) {
  force(commit_message)
  cli::rule("Commiting updated site", line = 1)
  with_dir(dest_dir, {
    git("add", "-A", ".")
    git("commit", "--allow-empty", "-m", commit_message)
    cli::rule("Deploying to GitHub Pages", line = 1)
    git("remote", "-v")
    git("push", "--force", remote, paste0("HEAD:", branch))
  })
}


updated_ee_version <- function(dest_dir) {
  oauth_func_path <- system.file("python/ee_utils.py", package = "rgee")
  ee_utils_py <- ee_source_python(oauth_func_path)
  py_version <- rgee::ee_utils_py_to_r(ee_utils_py$ee_getversion())
  file_path <- sprintf("%s/R/ee_version.R", dest_dir)
  message(file_path)
  fileConn <- file(file_path)
  writeLines(
    text = c(
      "#' Earth Engine API version",
      "#'",
      "#' This function returns the Earth Engine Python API",
      "#' version with which rgee was built.",
      "#' @export",
      "ee_version <- function() {",
      sprintf(" '%s'", py_version),
      "}"
    ),
    con = fileConn
  )
  close(fileConn)
}

updated_ee_README <- function(dest_dir) {
  oauth_func_path <- system.file("python/ee_utils.py", package = "rgee")
  ee_utils_py <- ee_source_python(oauth_func_path)
  py_version <- rgee::ee_utils_py_to_r(ee_utils_py$ee_getversion())
  file_path <- sprintf("%s/README.md", dest_dir)
  message(file_path)
  readme <- readLines(file_path, -1)
  readme[18] <- sprintf(
    "[earthengine-api %s](https://pypi.org/project/earthengine-api/%s/).",
    py_version,
    py_version
  )
  writeLines(readme, file_path)
}

updated_ee_DESCRIPTION <- function(dest_dir) {
  oauth_func_path <- system.file("python/ee_utils.py", package = "rgee")
  ee_utils_py <- ee_source_python(oauth_func_path)
  py_version <- rgee::ee_utils_py_to_r(ee_utils_py$ee_getversion())
  file_path <- sprintf("%s/DESCRIPTION", dest_dir)
  message(file_path)
  readme <- readLines(file_path, -1)
  readme[82] <- sprintf(
    '      list(package = "earthengine-api", version = "%s", pip = TRUE),',
    py_version
  )
  writeLines(readme, file_path)
}


update_rgee <- function(pkg = ".",
                        commit_message = construct_commit_message(pkg),
                        branch = "dev", remote = "origin") {
  # dest_dir <- fs::dir_create(fs::file_temp())
  # on.exit(fs::dir_delete(dest_dir))
  # git("remote", "set-branches", remote, branch)
  # git("fetch", remote, branch)
  # github_worktree_add(dest_dir, remote, branch)
  # on.exit(github_worktree_remove(dest_dir), add = TRUE)

  oauth_func_path <- system.file("python/ee_utils.py", package = "rgee")
  ee_utils_py <- ee_source_python(oauth_func_path)
  py_version <- rgee::ee_utils_py_to_r(ee_utils_py$ee_getversion())

  message("rgee version: ", rgee::ee_version())
  message("earthengine-api version: ", py_version)

  if (isFALSE(py_version == rgee::ee_version())) {
    # updated_ee_version(dest_dir)
    # updated_ee_README(dest_dir)
    # updated_ee_DESCRIPTION(dest_dir)
    # github_push(dest_dir, commit_message, remote, branch)
    # git("fetch", remote, branch)
    # rcmdcheck::rcmdcheck(
    #   args = c("--no-manual", "--as-cran"),
    #   error_on = "warning",
    #   check_dir = "check"
    # )
    stop("rgee not updated")
  }
  invisible(TRUE)
}

update_rgee()
