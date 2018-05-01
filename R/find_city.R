#' Detect prefecture by coordinates
#'
#' @param longitude longitude
#' @param latitude latitude
#' @param ... export parameter to other functions
#' @importFrom dplyr mutate select right_join
#' @importFrom purrr set_names
#' @note The `find_pref` function was added in version 0.3.0
#' @examples
#' \dontrun{
#' find_pref(longitude = 130.4412895, latitude = 30.2984335)
#' }
#' @export
find_pref <- function(longitude, latitude, ...) {
  prefecture <- city_code <- NULL

  res <- find_city(longitude, latitude, ...)

  if (!is.null(res)) {
    df_tmp <-
      res %>%
      dplyr::mutate(pref_code = substr(city_code, 1, 2)) %>%
      dplyr::select(pref_code, prefecture)

    res <- df_tmp %>%
      dplyr::right_join(
        jpn_pref(pref_code = df_tmp$pref_code, district = FALSE) %>%
          sf::st_set_geometry(NULL),
        by = c("pref_code" = "jis_code", "prefecture")
      ) %>%
      purrr::set_names(c("pref_code", "prefecture", "geometry")) %>%
      dplyr::mutate(pref_code = sprintf("%02d", as.numeric(pref_code))) %>%
      tweak_sf_output()

    return(res)
  }
}

#' Detect prefectures by coordinates
#'
#' @param longitude longitude
#' @param latitude latitude
#' @importFrom jpmesh coords_to_mesh
#' @importFrom dplyr filter inner_join select
#' @importFrom purrr set_names
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

  res <-
    prefecture_mesh %>%
    as.data.frame() %>%
    dplyr::select(prefcode, meshcode) %>%
    dplyr::filter(meshcode == jpmesh::coords_to_mesh(longitude,
                                                     latitude,
                                                     mesh_size = "80km")) %>%
    dplyr::inner_join(jpnprefs,
                      by = c("prefcode" = "jis_code")) %>%
    purrr::set_names(
      c("pref_code", "meshcode_80km", "prefecture", "region")) %>%
    tibble::as_tibble()

  return(res)
}

#' Detect city by coordinates
#'
#' @param longitude longitude
#' @param latitude latitude
#' @param ... export parameter to other functions
#' @importFrom dplyr select
#' @note The `find_city` function was added in version 0.3.0
#' @examples
#' \dontrun{
#' find_city(longitude = 140.1137418, latitude = 36.0533957)
#' }
#' @export
find_city <- function(longitude, latitude, ...) {
  prefecture <- city_code <- city <- geometry <- NULL

  pol_min <- which_pol_min(longitude, latitude, ...)

  if (identical(pol_min$which, integer(0)) == TRUE) {
    # not found
    message(intToUtf8(
      c(
        25351,
        23450,
        12375,
        12383,
        24231,
        27161,
        12364,
        12509,
        12522,
        12468,
        12531,
        12395,
        21547,
        12414,
        12428,
        12414,
        12379,
        12435
      )
    ))
  } else {
    res <- pol_min$spdf[pol_min$which, ] %>%
      dplyr::select(prefecture, city_code, city, geometry)

    return(res)
  }
}
