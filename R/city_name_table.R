#' Find prefecture code
#'
#' @inheritParams jpn_pref
#' @inheritParams jpn_cities
#' @param strict matching patterns
#' @return Prefecture Identification Code (JIS X 0401)
#' @examples
#' \dontrun{
#' find_jis_code(33, "\u5009\u6577\u5e02")
#' find_jis_code(33, "\u5009\u6577\u5e02", strict = FALSE)
#' }
find_jis_code <- function(pref_code, admin_name, strict = TRUE) {

  city <- city_code <- NULL

  pref <- rlang::quo(pref_code)

  # REVIEW:
  sf_pref <-
    jpn_pref(pref_code = rlang::eval_tidy(pref))

  if (rlang::is_true(strict)) {
    sf_pref <-
      sf_pref %>%
      dplyr::filter(city %in% c(admin_name))
  } else {
    sf_pref <-
      sf_pref %>%
      dplyr::filter(grepl(c(paste0(admin_name, collapse = "|")), city))
  }

  if (nrow(sf_pref) == 0) {
    sf_pref <-
      NA_character_
    rlang::warn("matching code were not found.")
    return(sf_pref)
  }

  sf_pref %>%
    dplyr::pull(city_code)

}

cityname_reform <- function(admin_name) {

  if (rlang::is_true(grepl("^.+\u90e1.+(\u753a|\u6751)$", admin_name)) &&
      !grepl("^.+\u90e1[:space:].+(\u753a|\u6751)$", admin_name)) {
    strsplit(admin_name, split = "\u90e1") %>%
      purrr::map(~ gsub("[[:space:]]", "", .x)) %>%
      purrr::flatten() %>%
      purrr::map_at(1, ~ paste0(., "\u90e1")) %>%
      purrr::reduce(paste)
  } else {
    admin_name
  }
}
