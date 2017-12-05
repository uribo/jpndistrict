#' Detect prefecture by coordinates
#'
#' @param longitude longitude
#' @param latitude latitude
#' @import jpmesh
#' @importFrom dplyr inner_join mutate mutate_if select
#' @importFrom tibble data_frame
#' @note The \code{find_pref} function was added in version 0.3.0
#' @examples
#' \dontrun{
#' find_pref(longitude = ,130.4412895, latitude = 30.2984335)
#' }
#' @export
find_pref <- function(longitude, latitude) {

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
                          dplyr::select(pref_name = prefecture,
                                        region) %>%
                          dplyr::mutate(pref_name = as.character(pref_name)),
                        by = c("prefecture" = "pref_name"))

    res <- tibble::data_frame(
      pref_code = geos$pref_code,
      prefecture = geos$prefecture,
      region    = geos$region
    )

  }
  return(res)

}


#' Detect prefectures by coordinates
#'
#' @param longitude longitude
#' @param latitude latitude
#' @importFrom jpmesh coords_to_mesh
#' @importFrom dplyr filter inner_join select
#' @importFrom tibble as_tibble
#' @examples
#' \dontrun{
#' find_prefs(longitude = 122.940625, latitude = 24.4520833334)
#' find_prefs(longitude = 140.1137418, latitude = 36.0533957)
#' }
#' @name find_prefs
#' @export
find_prefs <- function(longitude, latitude) {

  prefcode <- jis_code <- meshcode <- prefecture <- region <- NULL

  jpnprefs <- jpnprefs %>%
    dplyr::select(jis_code, prefecture, region)

  res <- prefecture_mesh %>%
    as.data.frame() %>%
    dplyr::select(prefcode, meshcode) %>%
    dplyr::filter(meshcode == jpmesh::coords_to_mesh(longitude, latitude, mesh_size = "80km")) %>%
    dplyr::inner_join(jpnprefs,
                      by = c("prefcode" = "jis_code")) %>%
    set_names(c("pref_code", "meshcode_80km", "prefecture", "region")) %>%
    tibble::as_tibble()

  return(res)
}

#' Detect city by coordinates
#'
#' @param longitude longitude
#' @param latitude latitude
#' @param ... path to arguments \code{spdf_jpn_pref}.
#' @importFrom tibble data_frame
#' @note The \code{find_city} function was added in version 0.3.0
#' @examples
#' \dontrun{
#' find_city(longitude = 140.1137418, latitude = 36.0533957)
#' }
#' @export
find_city <- function(longitude, latitude, ...) {

  pol_min <- which_pol_min(longitude = longitude, latitude = latitude)

  if (identical(pol_min$which, integer(0)) == TRUE) {
    # not found
    message(intToUtf8(c(25351, 23450, 12375, 12383, 24231, 27161, 12364, 12509, 12522, 12468, 12531, 12395, 21547, 12414, 12428, 12414, 12379, 12435)))
  } else {
    geos <- pol_min$spdf[pol_min$which, ]

    res <- tibble::data_frame(
      prefecture = geos$prefecture,
      city_code = geos$city_code,
      city = geos$city
    )

    return(res)
  }
}
