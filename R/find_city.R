#' Detect prefecture by coordinates
#'
#' @inheritParams find_city
#' @importFrom dplyr mutate select right_join
#' @importFrom purrr set_names
#' @importFrom sf st_is st_point
#' @note The `find_pref` function was added in version 0.3.0
#' @examples
#' \dontrun{
#' find_pref(longitude = 130.4412895, latitude = 30.2984335)
#'
#' # Refrenced by sf geometry
#' library(sf)
#' find_pref(geometry = st_point(c(136.6833, 35.05)))
#' }
#' @export
find_pref <- function(longitude, latitude, geometry = NULL, ...) {
  pref_code <- prefecture <- city_code <- NULL

  if (!is.null(geometry)) {
    if (sf::st_is(geometry, "POINT")) {
      coords <-
        sfg_point_as_coords(geometry = geometry)
      longitude <- coords$longitude
      latitude <- coords$latitude
    }
  }

  res <- find_city(longitude, latitude, ...)
  if (!is.null(res)) {
    df_tmp <- res %>%
      dplyr::mutate(pref_code = substr(city_code, 1, 2)) %>%
      dplyr::select(pref_code, prefecture)
    res <- df_tmp %>%
      dplyr::right_join(jpn_pref(pref_code = df_tmp$pref_code,
                                 district = FALSE) %>%
                          sf::st_set_geometry(NULL),
                        by = c(pref_code = "jis_code", "prefecture")) %>%
      purrr::set_names(c("pref_code", "prefecture", "geometry")) %>%
      dplyr::mutate(pref_code = sprintf("%02d", as.numeric(pref_code))) %>%
      tweak_sf_output()
    return(res)
  }
}

#' Detect prefectures by coordinates
#'
#' @inheritParams find_city
#' @importFrom jpmesh coords_to_mesh
#' @importFrom dplyr filter inner_join select
#' @importFrom purrr set_names
#' @importFrom tibble as_tibble
#' @examples
#' \dontrun{
#' find_prefs(longitude = 122.940625, latitude = 24.4520833334)
#' find_prefs(longitude = 140.1137418, latitude = 36.0533957)
#'
#' # Refrenced by sf geometry
#' library(sf)
#' find_pref(geometry = st_point(c(136.6833, 35.05)))
#' }
#' @name find_prefs
#' @export
find_prefs <- function(longitude, latitude, geometry = NULL) {
  prefcode <- jis_code <- meshcode <- prefecture <- region <- NULL

  if (!is.null(geometry)) {
    if (sf::st_is(geometry, "POINT")) {
      coords <-
        sfg_point_as_coords(geometry = geometry)
      longitude <- coords$longitude
      latitude <- coords$latitude
    }
  }

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
#' @param geometry XY sfg object
#' @param ... export parameter to other functions
#' @importFrom dplyr select
#' @note The `find_city` function was added in version 0.3.0
#' @examples
#' \dontrun{
#' find_city(longitude = 140.1137418, latitude = 36.0533957)
#'
#' # Refrenced by sf geometry
#' library(sf)
#' find_city(geometry = st_point(c(136.6833, 35.05)))
#' }
#' @export
find_city <- function(longitude, latitude, geometry = NULL, ...) {
  prefecture <- city_code <- city <- NULL

  if (!is.null(geometry)) {
    if (sf::st_is(geometry, "POINT")) {
      coords <-
        sfg_point_as_coords(geometry = geometry)
      longitude <- coords$longitude
      latitude <- coords$latitude
    }
  }

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
