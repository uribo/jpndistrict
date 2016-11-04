#' Spatial Data frame
#' @import dplyr
#' @import magrittr
#' @importFrom readr read_rds
#' @param code jis code
#' @param admin_name string
#' @param district logical (default TRUE)
#' @examples
#' spdf_jpn_pref(admin_name = "岡山県", district = FALSE)
#' spdf_jpn_pref(code = 33, district = FALSE)
#' @export
spdf_jpn_pref <- function(code = NULL, admin_name = NULL, district = TRUE) {

  if (missing(admin_name)) {
    pref.code <- dplyr::filter_(jpnprefs, ~jis_code == pref_code(code)) %>% magrittr::use_series(jis_code)
  } else if (missing(code)) {
    pref.code <- dplyr::filter_(jpnprefs, ~prefecture == admin_name) %>% magrittr::use_series(jis_code)
  }

  if (district == TRUE) {
    d <- readr::read_rds(system.file(paste0("extdata/pref_", pref.code, "_city_spdf.rds"), package = "jpndistrict"))
  } else {
    d <- readr::read_rds(system.file(paste0("extdata/pref_", pref.code, "_spdf.rds"), package = "jpndistrict"))
  }
  return(d)
}

#' Spatial Data frame city single
#' @import spdplyr
#' @importFrom dplyr filter
#' @param df data frame
#' @param jis_code jis code
#' @param admin_name administration name (full lenght)
#' @examples
#' spdf_jpn_city(spdf_jpn_pref(code = 33, district = TRUE), jis_code = 33103)
#' spdf_jpn_city(spdf_jpn_pref(code = 33, district = TRUE), admin_name = "倉敷市")
#' @export
spdf_jpn_city <- function(df, jis_code, admin_name = NULL) {

  if (missing(admin_name)) {
    d <- dplyr::filter(df, city_code == jis_code)
  } else if (missing(jis_code)) {
    d <- dplyr::filter(df, city_name_full == admin_name)
  }
  return(d)
}

#' Spatial Data frame cities
#' @import spdplyr
#' @importFrom dplyr filter
#' @param jis_code jis code
#' @param admin_name string
#' @param jis_code_pref numeric
#' @examples
#' spdf_jpn_cities(jis_code_pref = 33, jis_code = 33103)
#' spdf_jpn_cities(jis_code_pref = 33, jis_code = c(33103, 33104, 33205))
#' @export
spdf_jpn_cities <- function(jis_code_pref, jis_code = NULL, admin_name = NULL) {
  d <- spdf_jpn_pref(jis_code_pref, district = TRUE)

  if (missing(admin_name)) {
    d <- dplyr::filter(d, city_code %in% jis_code)
  } else if (missing(jis_code)) {
    d <- dplyr::filter(d, city_name_full %in% admin_name)
  }

  return(d)
}
