do_package_checks(error_on = "error")

if (ci_on_travis()) {
  tic::do_pkgdown(path = "docs", branch = "master")
}
