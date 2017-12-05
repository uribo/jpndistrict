.onAttach <- function(...) {
  packageStartupMessage(paste(strwrap(creditmsg), collapse = "\n"))
  #suppressPackageStartupMessages(strwrap(creditmsg))

  # msg(tidyverse_conflict_message(x), startup = TRUE)
}
