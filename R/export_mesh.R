#' Export district's mesh polygon
#' @inheritParams code_validate
#' @param to_mesh_size target mesh type. From 80km to 1km as numeric.
#' @examples
#' mesh_district(jis_code = "33101", to_mesh_size = 80)
#' mesh_district(jis_code = "05", to_mesh_size = 80)
#' @export
mesh_district <- memoise::memoise(
  function(jis_code = NULL, to_mesh_size = NULL) {
    to_mesh_size <-
      as.character(to_mesh_size)
    to_mesh_size <-
      rlang::arg_match(to_mesh_size,
                     c("80", "10", "1"))
    input_code <-
      code_validate(jis_code)
  if (input_code$administration_type == "prefecture") {
    sf_admins <-
      jpn_pref(pref_code = input_code$code)
  } else if (input_code$administration_type == "city") {
    sf_admins <-
      jpn_cities(jis_code = input_code$code)
  }
    res <-
      mesh_intersect(jpmesh::sf_jpmesh, sf_admins)
    if (to_mesh_size == "80") {
      res %>%
        dplyr::select(meshcode, geometry)
    } else {
      res <-
        res %>%
        mesh_intersect_filter() %>%
        mesh_intersect(sf_admins)
      if (to_mesh_size == "10") {
        res
      } else {
        res %>%
          mesh_intersect_filter() %>%
          mesh_intersect(sf_admins)
      }
    }
  }
)
