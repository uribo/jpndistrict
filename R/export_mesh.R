#' Export district's mesh polygon
#'
#' @inheritParams code_validate
#' @importFrom dplyr filter mutate select everything pull
#' @importFrom jpmesh fine_separate mesh_to_coords
#' @importFrom purrr map set_names pmap
#' @importFrom sf st_intersects st_sf
#' @importFrom tibble as_data_frame tibble
#' @importFrom tidyr unnest
#' @examples
#' \dontrun{
#' mesh_district(jis_code = "05")
#' mesh_district(jis_code = 33101)
#' }
#' @export
mesh_district <- function(jis_code = NULL) {

  . <- res_contains <- meshcode <- NULL

  input_code <-
    code_validate(jis_code)

  if (input_code$administration_type == "prefecture") {
    sf_admins <- jpn_pref(pref_code = input_code$code)
  } else if (input_code$administration_type == "city") {
    sf_admins <- jpn_cities(jis_code = input_code$code)
  }

  df_tmp <- tibble::tibble(
    res_contains = suppressMessages(sf::st_intersects(jpmesh::sf_jpmesh,
                                                      sf_admins,
                                                      sparse = FALSE) %>% rowSums()))

  df_tmp$id <- 1:nrow(df_tmp)

  df_pref10km_mesh <- jpmesh::sf_jpmesh[df_tmp %>%
                                          dplyr::filter(res_contains != 0) %>%
                                          tidyr::unnest() %>%
                                          dplyr::pull(id) %>%
                                          unique(), ] %>%
    .$meshcode %>%
    purrr::map(jpmesh::fine_separate) %>%
    rlang::flatten_chr() %>%
    unique() %>%
    tibble::as_tibble() %>%
    purrr::set_names("meshcode") %>%
    dplyr::mutate(out = purrr::pmap(., ~ jpmesh::mesh_to_coords(...))) %>%
    tidyr::unnest() %>%
    dplyr::select(meshcode, dplyr::everything()) %>%
    dplyr::mutate(geometry = purrr::pmap(., ~ jpmesh:::mesh_to_poly(...))) %>%
    sf::st_sf(crs = 4326, stringsAsFactors = FALSE)

  df_tmp <- tibble::tibble(
    res_contains = suppressMessages(sf::st_intersects(df_pref10km_mesh,
                                                      sf_admins,
                                                      sparse = FALSE) %>% rowSums()))
  df_tmp$id <- 1:nrow(df_tmp)

  df_pref1km_mesh <-
    df_pref10km_mesh[df_tmp %>%
                dplyr::filter(res_contains != 0) %>%
                       tidyr::unnest() %>%
                       dplyr::pull(id) %>%
                       unique(), ] %>%
    .$meshcode %>%
    purrr::map(jpmesh::fine_separate) %>%
    rlang::flatten_chr() %>%
    unique() %>%
    tibble::as_tibble() %>%
    purrr::set_names("meshcode") %>%
    dplyr::mutate(out = purrr::pmap(., ~ jpmesh::mesh_to_coords(...))) %>%
    tidyr::unnest() %>%
    dplyr::select(meshcode, dplyr::everything()) %>%
    dplyr::mutate(geometry = purrr::pmap(., ~ jpmesh:::mesh_to_poly(...))) %>%
    sf::st_sf(crs = 4326, stringsAsFactors = FALSE)

  df_tmp <- tibble::tibble(
    res_contains = suppressMessages(sf::st_intersects(df_pref1km_mesh,
                                                      sf_admins,
                                                      sparse = FALSE) %>% rowSums()))
  df_tmp$id <- 1:nrow(df_tmp)

  df_pref1km_mesh[df_tmp %>%
                     dplyr::filter(res_contains != 0) %>%
                     tidyr::unnest() %>%
                     dplyr::pull(id) %>%
                     unique(), ]

}
