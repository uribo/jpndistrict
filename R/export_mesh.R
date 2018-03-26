#' Export district's mesh polygon
#'
#' @param jis_code jis code for prefecture and city identifical number
#' @importFrom dplyr mutate select everything
#' @importFrom jpmesh fine_separate mesh_to_coords
#' @importFrom magrittr use_series
#' @importFrom purrr map set_names pmap
#' @importFrom sf st_intersects st_sf
#' @importFrom tibble as_data_frame tibble
#' @importFrom tidyr unnest
#' @examples
#' \dontrun{
#' mesh_district(jis_code = 33101)
#' }
#' @export
mesh_district <- function(jis_code = NULL) {

  . <- meshcode <- NULL

  sf_pref <- jpn_cities(jis_code = jis_code)

  df_tmp <- tibble::tibble(
    res_contains = suppressMessages(sf::st_intersects(jpmesh::sf_jpmesh,
                                                      sf_pref) %>% as.numeric()))

  df_tmp$id <- 1:nrow(df_tmp)

  df_pref10km_mesh <- jpmesh::sf_jpmesh[df_tmp %>%
                                          tidyr::unnest() %>%
                                          magrittr::use_series(id) %>%
                                          unique(), ] %>%
    .$meshcode %>%
    purrr::map(jpmesh::fine_separate) %>%
    rlang::flatten_chr() %>%
    unique()

  sf_prefmesh <- df_pref10km_mesh %>%
    tibble::as_tibble() %>%
    purrr::set_names("meshcode") %>%
    dplyr::mutate(out = purrr::pmap(., ~ jpmesh::mesh_to_coords(...))) %>%
    tidyr::unnest() %>%
    dplyr::select(meshcode, dplyr::everything()) %>%
    dplyr::mutate(geometry = purrr::pmap(., ~ jpmesh:::mesh_to_poly(...))) %>%
    sf::st_sf(crs = 4326)

  df_tmp <- tibble::tibble(
    res_contains = suppressMessages(sf::st_intersects(sf_prefmesh,
                                                      sf_pref) %>% as.numeric()))
  df_tmp$id <- 1:nrow(df_tmp)

  sf_prefmesh[df_tmp %>%
                       tidyr::unnest() %>%
                       magrittr::use_series(id) %>%
                       unique(), ] %>%
    tibble::as_tibble() %>%
    sf::st_sf(crs = 4326)

}
