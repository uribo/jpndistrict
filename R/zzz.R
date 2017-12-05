.onAttach <- function(...) {
  packageStartupMessage(paste(strwrap(creditmsg), collapse = "\n"))
}
