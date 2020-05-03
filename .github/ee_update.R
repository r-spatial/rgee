git <- function(..., echo_cmd = TRUE, echo = TRUE, error_on_status = TRUE) {
  processx::run("git", c(...), echo_cmd = echo_cmd, echo = echo,
                error_on_status = error_on_status)
}

git_has_remote_branch <- function(remote, branch) {
  has_remote_branch <- git(
    "ls-remote",
    "--quiet",
    "--exit-code",
    remote, branch, echo = FALSE, echo_cmd = FALSE, error_on_status = FALSE)$status == 0
}

old_branch <-function () {
  branch <- git("rev-parse", "--abbrev-ref", "HEAD", echo = FALSE,
                echo_cmd = FALSE)$stdout
  sub("\n$", "", branch)
}

ci_commit_sha <- function () {
  env_vars <- c("TRAVIS_COMMIT", "GITHUB_SHA")
  for (var in env_vars) {
    commit_sha <- Sys.getenv(var, "")
    if (commit_sha != "")
      return(commit_sha)
  }
  ""
}

construct_commit_message <- function (pkg, commit = ci_commit_sha()) {
  rgee_version <- packageVersion("rgee")
  sprintf("Update rgee: %s@%s", rgee_version,
          substr(commit, 1, 7))
}

github_worktree_add <- function (dir, remote, branch) {
  rule("Adding worktree", line = 1)
  git("worktree", "add", "--track", "-B", branch, dir, paste0(remote, "/", branch))
}

github_worktree_remove <- function (dir) {
  rule("Removing worktree", line = 1)
  git("worktree", "remove", dir)
}

with_dir <- function(new, code) {
  old <- setwd(dir = new)
  on.exit(setwd(old))
  force(code)
}

github_push <- function(dir, commit_message, remote, branch) {
  force(commit_message)
  cli::rule("Commiting updated site", line = 1)
  with_dir(dir, {
    git("add", "-A", ".")
    git("commit", "--allow-empty", "-m", commit_message)
    cli::rule("Deploying to GitHub Pages", line = 1)
    git("remote", "-v")
    git("push", "--force", remote, paste0("HEAD:", branch))
  })
}


updated_ee_version <- function() {
  fileConn <- file("R/ee_version.R")
  writeLines(
    text = c(
      "#' Earth Engine API version",
      "#'",
      "#' This function returns the Earth Engine Python API",
      "#' version with which rgee was built.",
      "#' @export",
      "ee_version <- function() {",
      sprintf(" '%s'", ee_utils_py$ee_getversion()),
      "}"),
    con = fileConn
  )
  close(fileConn)
}

updated_ee_README <- function() {
  readme = readLines("README.md",-1)
  readme[18] = sprintf(
    "[earthengine-api %s](https://pypi.org/project/earthengine-api/%s/).",
    ee_utils_py$ee_getversion(),
    ee_utils_py$ee_getversion()
  )
  writeLines(readme,"README.md")
}

update_rgee <- function(pkg = ".",
                         commit_message = construct_commit_message(pkg),
                         branch = "master", remote = "origin") {
  dest_dir <- fs::dir_create(fs::file_temp())
  on.exit(fs::dir_delete(dest_dir))
  git("remote", "set-branches", remote, branch)
  git("fetch", remote, branch)
  github_worktree_add(dest_dir, remote, branch)
  on.exit(github_worktree_remove(dest_dir), add = TRUE)

  oauth_func_path <- system.file("python/ee_utils.py", package = "rgee")
  ee_utils_py <- rgee:::ee_source_python(oauth_func_path)

  if (!ee_utils_py$ee_getversion() == rgee:::ee_version()) {
    updated_ee_version()
    updated_ee_README()
  }

  github_push(dest_dir, commit_message, remote, branch)
  invisible()
}

update_rgee()
