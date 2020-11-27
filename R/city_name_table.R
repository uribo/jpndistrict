#' Find JIS city code
#'
#' @inheritParams jpn_pref
#' @inheritParams jpn_cities
#' @param strict matching patterns
#' @return Identification code for cities, towns and villages (JIS X 0402:2010)
#' @examples
#' find_jis_code(33, intToUtf8(c(20489, 25975, 24066), multiple = FALSE))
#' find_jis_code(33, enc2native(intToUtf8(c(20489, 25975, 24066),
#'                                        multiple = FALSE)),
#'                                        strict = FALSE)
#' find_jis_code(14,
#' c(enc2native(intToUtf8(c(37772, 20489, 24066), multiple = FALSE)),
#'   enc2native(intToUtf8(c(23567, 30000, 21407, 24066), multiple = FALSE))), strict = FALSE) # nolint
#' }
find_jis_code <- function(pref_code, admin_name, strict = TRUE) {
  city <- city_code <- NULL
  pref <- rlang::quo(pref_code)
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
  } else if (rlang::is_true(grepl("^.+\u5e02.+\u533a$", admin_name))) {
    strsplit(admin_name, split = "\u5e02") %>%
      purrr::flatten_chr() %>%
      purrr::map_chr(~ gsub("[[:space:]]", "", x = .x)) %>%
      paste(collapse = "\u5e02 ")
  } else {
    admin_name
  }
}
