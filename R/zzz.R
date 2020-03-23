.onAttach <- function(...) { # nolint
  packageStartupMessage(paste(strwrap(creditmsg), collapse = "\n"))
}
