#' Export district's mesh polygon
#'
#' @inheritParams code_validate
#' @importFrom dplyr filter mutate select pull
#' @importFrom jpmesh fine_separate mesh_to_coords
#' @importFrom purrr map set_names pmap
#' @importFrom sf st_intersects st_sf
#' @importFrom tibble as_data_frame tibble
#' @importFrom tidyr unnest
#' @importFrom tidyselect everything
#' @examples
#' \dontrun{
#' mesh_district(jis_code = "05")
#' mesh_district(jis_code = "33101")
#' }
#' @export
mesh_district <- function(jis_code = NULL) {
  input_code <-
    code_validate(jis_code)
  if (input_code$administration_type == "prefecture") {
    sf_admins <- jpn_pref(pref_code = input_code$code)
  } else if (input_code$administration_type == "city") {
    sf_admins <- jpn_cities(jis_code = input_code$code)
  }
  mesh_intersect(jpmesh::sf_jpmesh, sf_admins) %>%
    mesh_intersect_filter() %>%
    mesh_intersect(sf_admins) %>%
    mesh_intersect_filter() %>%
    mesh_intersect(sf_admins)
}
