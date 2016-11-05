#' Get prefecture code from jis code
#' @param jis_code jis code
pref_code <- function(jis_code) {
  res <- sprintf("%02d", as.numeric(substr(jis_code, 1, 2)))
  return(res)
}

#' Collect administration office point datasets.
#'
#' @param code prefecture code
#' @param path shepfile directory
#' @import dplyr
#' @import magrittr
#' @importFrom geojsonio geojson_read
#' @importFrom stringi stri_trim_both
collect_ksj_p33 <- function(code = NULL, path = NULL) {

  d <- geojsonio::geojson_read(
    paste0(path, "/", list.files(path, pattern = paste0(code, ".shp$"))),
    method = "local",
    what = "sp",
    stringsAsFactors = TRUE,
    encoding = "cp932")

  d@data <- d@data %>%
    bind_cols(as.data.frame(d@coords)) %>%
    set_colnames(c("jis_code", "type", "name", "address", "longitude", "latitude")) %>%
    rowwise() %>%
    mutate(address = stringi::stri_trim_both(address)) %>%
    ungroup()

  return(d)

}
