#' Detect prefecture by coordinates
#'
#' @inheritParams find_city
#' @note The `find_pref` function was added in version 0.3.0
#' @examples
#' \dontrun{
#' find_pref(longitude = 130.4412895, latitude = 30.2984335)
#'
#' # Referenced by sf geometry
#' library(sf)
#' find_pref(geometry = st_point(c(130.4412895, 30.2984335)))
#' }
#' @export
find_pref <- function(longitude, latitude, geometry = NULL, ...) {
  . <- pref_code <- prefecture <- city_code <- dist <- NULL # nolint
  if (rlang::is_false(rlang::is_null(geometry))) {
    if (sf::st_is(geometry, "POINT")) {
      coords <-
        sfg_point_as_coords(geometry = geometry)
      longitude <- coords$longitude
      latitude <- coords$latitude
    }
  }
  res <-
    find_city(longitude, latitude, ...)
  if (rlang::is_false(rlang::is_null(res))) {
    if (nrow(res) > 1) {
      res <-
        res %>%
        dplyr::mutate(dist = purrr::pmap_dbl(
          ., ~
            sf::st_distance(sf::st_centroid(..4) %>%
                              sf::st_sfc(crs = 4326),
                            sf::st_point(c(longitude, latitude)) %>%
                              sf::st_sfc(crs = 4326),
                            by_element = TRUE)))
      res <-
        res %>%
        dplyr::arrange(dist) %>%
        dplyr::slice(1L) %>%
        dplyr::select(-dist)
    }
    res <-
      jpn_pref(pref_code = substr(res$city_code, 1, 2),
                    district = FALSE) %>%
      dplyr::mutate(pref_code = sprintf("%02d", as.numeric(pref_code))) %>%
      tweak_sf_output()
    return(res)
  }
}

#' Detect prefectures by coordinates
#'
#' @inheritParams find_city
#' @examples
#' find_prefs(longitude = 122.940625, latitude = 24.4520833334)
#' find_prefs(longitude = 140.1137418, latitude = 36.0533957)
#'
#' # Referenced by sf geometry
#' library(sf)
#' find_pref(geometry = st_point(c(136.6833, 35.05)))
#' @name find_prefs
#' @export
find_prefs <- function(longitude, latitude, geometry = NULL) {
  prefcode <- jis_code <- meshcode <- prefecture <- region <- NULL
  if (rlang::is_false(rlang::is_null(geometry))) {
    if (sf::st_is(geometry, "POINT")) {
      coords <-
        sfg_point_as_coords(geometry = geometry)
      longitude <- coords$longitude
      latitude <- coords$latitude
    }
  }
  jpnprefs <-
    jpnprefs %>%
    dplyr::select(jis_code, prefecture, region)
  res <-
    prefecture_mesh %>%
    dplyr::select(prefcode, meshcode) %>%
    dplyr::filter(meshcode == jpmesh::coords_to_mesh(longitude = longitude,
                                                     latitude = latitude,
                                                     mesh_size = 80)) %>%
    dplyr::inner_join(jpnprefs,
                      by = c("prefcode" = "jis_code")) %>%
    dplyr::relocate(geometry, .after = region) %>%
    purrr::set_names(
      c("pref_code", "meshcode_80km", "prefecture", "region", "geometry"))
  return(res)
}

#' Detect city by coordinates
#'
#' @param longitude longitude
#' @param latitude latitude
#' @param geometry XY sfg object
#' @param ... export parameter to other functions
#' @note The `find_city` function was added in version 0.3.0
#' @examples
#' find_city(longitude = 140.1137418, latitude = 36.0533957)
#'
#' # Referenced by sf geometry
#' library(sf)
#' find_city(geometry = st_point(c(136.6833, 35.05)))
#' @export
find_city <- memoise::memoise(
  function(longitude, latitude, geometry = NULL, ...) {
    prefecture <- city_code <- city <- NULL
    if (!is.null(geometry)) {
      if (sf::st_is(geometry, "POINT")) {
        coords <-
          sfg_point_as_coords(geometry = geometry)
        longitude <- coords$longitude
        latitude <- coords$latitude
      }
    }
    pol_min <-
      which_pol_min(longitude, latitude, ...)
    if (identical(pol_min$which, integer(0)) == TRUE) {
      # not found
      rlang::inform("Specified coordinates are not included in the polygon.")
    } else {
      res <-
        pol_min$spdf[pol_min$which, ] %>%
        dplyr::select(prefecture, city_code, city, geometry)
      return(res)
    }
  }
)
