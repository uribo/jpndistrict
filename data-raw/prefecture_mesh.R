devtools::load_all(".")
library(tidyverse)
library(sf)
library(testthat)

data("sf_jpmesh", package = "jpmesh")

export_pref_80km_mesh <- function(code,...) {
  sf_pref <- jpndistrict::jpn_pref(pref_code = code)

  res <- sf_jpmesh[tibble::tibble(res_contains = suppressMessages(sf::st_overlaps(sf_jpmesh,
                                                                                  sf_pref))) %>%
                     mutate(id = row_number()) %>%
                     tidyr::unnest() %>%
                     use_series(id), ] %>%
    use_series(meshcode) %>%
    unique()

  return(res)
}
expect_length(export_pref_80km_mesh(1), 40L)

prefecture_mesh <- tibble::tibble(
  prefcode = sprintf("%02d", 1:47),
  meshcode = 1:47 %>%
    purrr::map(export_pref_80km_mesh)
) %>%
  tidyr::unnest() %>%
  left_join(sf_jpmesh %>% select(meshcode, starts_with("name"), type, geometry), by = "meshcode") %>%
  sf::st_sf(crs = 4326)

expect_s3_class(prefecture_mesh, c("tbl", "data.frame"))
expect_equal(dim(prefecture_mesh), c(314, 6))
expect_named(prefecture_mesh, c("prefcode", "meshcode", "name", "name_roman", "type", "geometry"))

# prefecture_mesh %>% count(meshcode, sort = TRUE)
# library(leaflet)
# pal <- colorFactor(palette = viridis::viridis(47),
#                    domain = stringr::str_pad(1:47, width = 2, pad = "0"))
# leaflet(data = prefecture_mesh %>% filter(prefcode == "01")) %>% addTiles() %>%
#   addPolygons(label = ~prefcode,
#               color = ~pal(prefcode),
#               labelOptions = labelOptions(noHide = TRUE, textsize = "24px"))

devtools::use_data(prefecture_mesh, overwrite = TRUE)
