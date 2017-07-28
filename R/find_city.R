#' Detect prefecture by coordinates
#'
#' @param lon longitude
#' @param lat latitude
#' @import jpmesh
#' @importFrom dplyr inner_join
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_if
#' @importFrom dplyr select
#' @importFrom tibble data_frame
#' @note The \code{find_pref} function was added in version 0.2.1
#' @export
find_pref <- function(lon = lon, lat = lat) {

  jis_code <- prefecture <- region <- pref_name <- NULL
  mesh <- jis_code <- prefecture <- region <- NULL

  pol_min <- which_pol_min(lon = lon, lat = lat)

  if (identical(pol_min$which, integer(0)) == TRUE) {
    res <- tibble::data_frame(
      pref_code = NA,
      pref_name = NA,
      region    = NA
    )
  } else {
    geos <- pol_min$spdf[pol_min$which, ] %>%
      dplyr::mutate_if(is.factor, as.character) %>%
      as.data.frame() %>%
      dplyr::inner_join(jpnprefs %>%
                          dplyr::select(pref_code = jis_code,
                                        pref_name = prefecture,
                                        region) %>%
                          dplyr::mutate(pref_name = as.character(pref_name)),
                        by = "pref_name")

    res <- tibble::data_frame(
      pref_code = geos$pref_code,
      pref_name = geos$pref_name,
      region    = geos$region
    )

  }
  return(res)

}

#' Detect prefectures by coordinates
#'
#' @param lon longitude
#' @param lat latitude
#' @import jpmesh
#' @importFrom dplyr filter inner_join select
#' @importFrom magrittr set_names
#' @name find_prefs
#' @export
find_prefs <- function(lon = lon, lat = lat) {

  mesh <- jis_code <- prefecture <- region <- NULL

  res <- jpmesh::prefecture_mesh %>%
    dplyr::filter(mesh == jpmesh::latlong_to_meshcode(lat = lat, long = lon, order = 1)) %>%
    dplyr::inner_join(jpnprefs %>%
                        dplyr::select(jis_code, prefecture, region), by = c("pref" = "jis_code")) %>%
    dplyr::mutate(prefecture = as.character(prefecture)) %>%
    magrittr::set_names(c("pref_code", "mesh_code", "pref_name", "region"))
  return(res)
}

#' Internal function
#'
#' @param lon longitude
#' @param lat latitude
#' @importFrom magrittr use_series
#' @importFrom purrr map reduce
#' @importFrom sf st_contains st_point
#' @name which_pol_min
which_pol_min <- function(lon = lon, lat = lat) {

  . <- NULL

  sp_polygon <- find_prefs(lon = lon, lat = lat) %>%
    magrittr::use_series(pref_code) %>%
    purrr::map(
      spdf_jpn_pref
    ) %>%
    purrr::reduce(rbind)

  which.row <- suppressMessages(sf::st_contains(sp_polygon,
                                                sf::st_point(c(lon, lat), dim = "XY"),
                                                sparse = FALSE)) %>%
    grep(TRUE, .)

  res <- list(spdf = sp_polygon, which = which.row)
  return(res)
}

#' Detect city by coordinates
#'
#' @param lon longitude
#' @param lat latitude
#' @param ... path to arguments \code{spdf_jpn_pref}.
#' @importFrom tibble data_frame
#' @note The \code{find_city} function was added in version 0.2.1
#' @export
find_city <- function(lon = lon, lat = lat, ...) {

  pol_min <- which_pol_min(lon = lon, lat = lat)

  if (identical(pol_min$which, integer(0)) == TRUE) {
    # not found
    message(intToUtf8(c(25351, 23450, 12375, 12383, 24231, 27161, 12364, 12509, 12522, 12468, 12531, 12395, 21547, 12414, 12428, 12414, 12379, 12435)))
  } else {
    geos <- pol_min$spdf[pol_min$which, ]

    res <- tibble::data_frame(
      pref_name = geos$pref_name,
      city_code = geos$city_code,
      city_name = geos$city_name_full
    )

    return(res)
  }
}
