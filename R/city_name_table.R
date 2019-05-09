#' Find jis city code
#'
#' @inheritParams jpn_pref
#' @inheritParams jpn_cities
#' @param strict matching patterns
#' @return Identification code for cities,towns and villages (JIS X 0402:2010)
#' @examples
#' \dontrun{
#' find_jis_code(33,
#' paste(enc2native(intToUtf8(c(20489, 25975, 24066),
#' multiple = TRUE)), collapse = ""))
#' find_jis_code(33,
#' paste(enc2native(intToUtf8(c(20489, 25975, 24066),
#' multiple = TRUE)), collapse = ""),
#' strict = FALSE)
#'
#' find_jis_code(14,
#' c(paste(enc2native(intToUtf8(c(37772, 20489, 24066), multiple = TRUE)),
#' collapse = ""),
#' paste(enc2native(intToUtf8(c(23567, 30000, 21407, 24066), multiple = TRUE)),
#' collapse = "")), strict = FALSE)
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

  if (rlang::is_true(
    grepl(paste0("^.+", enc2native(intToUtf8(37089)),
           ".+(", enc2native(intToUtf8(30010)),
           "|", enc2native(intToUtf8(26449)),
           ")$"), admin_name)) &&
      !grepl(
        paste0("^.+", enc2native(intToUtf8(37089)),
               "[:space:].+(", enc2native(intToUtf8(30010)),
               "|", enc2native(intToUtf8(26449)),
               ")$"), admin_name)) {
    strsplit(admin_name, split = enc2native(intToUtf8(37089))) %>%
      purrr::map(~ gsub("[[:space:]]", "", .x)) %>%
      purrr::flatten() %>%
      purrr::map_at(1, ~ paste0(., enc2native(intToUtf8(37089)))) %>%
      purrr::reduce(paste)
  } else if (rlang::is_true(grepl(
    paste0("^.+", enc2native(intToUtf8(24066)),
           ".+", enc2native(intToUtf8(21306)),
           "$"), admin_name))) {
    strsplit(admin_name, split = enc2native(intToUtf8(24066))) %>%
      purrr::flatten_chr() %>%
      purrr::map_chr(~ gsub("[[:space:]]", "", x = .x)) %>%
      paste(collapse = paste(enc2native(intToUtf8(c(24066, 32), multiple = TRUE)), collapse = ""))
  } else {
    admin_name
  }
}
