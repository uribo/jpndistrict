#' Administration code varidation
#'
#' @param jis_code jis code for prefecture and city identifical number.
#' If prefecture, must be from 1 to 47. If city, range of 5 digits.
#' @importFrom purrr flatten_chr keep map map_chr map_if
#' @note The `code_validate` function was added in version 0.3.2.9000
#' @examples
#' \dontrun{
#' code_validate(jis_code = "05")
#' code_validate(jis_code = 33101)
#' code_validate(jis_code = c("01", "33101"))
#' }
#' @export
code_validate <- function(jis_code) {

  . <- NULL

  res <-
    code_reform(jis_code)

  names(res) <-
    res %>%
    purrr::map(nchar) %>%
    purrr::map_chr(~ ifelse(.x == 2, "prefecture", "city"))

  res <-
    res %>%
    purrr::map_if(names(.) == "prefecture", ~ prefcode_validate(.x)) %>%
    purrr::map_if(names(.) == "city", ~ match_city_name(.x)$city_code) %>%
    purrr::keep(~ length(.x) == 1)

  list(administration_type = names(res),
       code = purrr::flatten_chr(res))
}

#' Reform input jis code as 2 or 5 character length.
#'
#' @inheritParams code_validate
#' @importFrom rlang abort
#' @importFrom purrr flatten_chr keep map map_if
#' @note The `code_reform` function was added in version 0.3.2.9000
#' @name code_reform
#' @examples
#' code_reform(c(1, "33", "08201"))
#' @export
code_reform <- function(jis_code) {
  . <- NULL

  checked <-
    jis_code %>%
    purrr::map(nchar) %>%
    purrr::keep(~ .x %in% c(1, 2, 5)) %>%
    length()

  if (length(jis_code) != checked)
    rlang::abort("Input jis-code must to 2 or 5 digits.")

  jis_code %>%
    purrr::map(as.numeric) %>%
    purrr::map_if(.p = nchar(.) %in% c(1, 2), ~ sprintf("%02d", .x)) %>%
    purrr::map_if(.p = nchar(.) %in% c(4, 5), ~ sprintf("%05d", .x)) %>%
    purrr::flatten_chr()
}

prefcode_validate <- function(pref_code) {
  codes <-
    sapply(seq(1, 47, 1), sprintf, fmt = "%02d")

  if (identical(codes[codes %in% pref_code], character(0)))
    rlang::abort("jis_code must be start a integer or as character from 1 to 47.")

  pref_code
}

match_city_name <- function(jis_code) {
  city_code <- city <- NULL

  df <-
    code_reform(jis_code) %>%
    purrr::map_chr(~ substr(.x, 1, 2)) %>%
    unique() %>%
    purrr::map_dfr(
      ~ jpn_pref(.x, district = TRUE) %>%
        sf::st_drop_geometry() %>%
        dplyr::select(city_code, city))

  res <-
    subset(df, city_code %in% jis_code)

  n_mismatch <-
    length(jis_code[!jis_code %in% res$city_code])

  if (n_mismatch >= 1)
    rlang::inform(
      paste(n_mismatch, "matching code were not found."))

  res
}
