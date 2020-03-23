# pkgload::load_all()
library(tidyverse)
library(sf)
library(assertr)
data("sf_jpmesh", package = "jpmesh")

prefecture_mesh <-
  tibble::tibble(
  prefcode = sprintf("%02d", seq_len(47)),
  meshcode = seq_len(47) %>%
    purrr::map(export_pref_80km_mesh)
) %>%
  tidyr::unnest(cols = meshcode) %>%
  left_join(sf_jpmesh %>% select(meshcode, name = name_roman, type, geometry), by = "meshcode") %>%
  st_sf(crs = 4326) %>%
  mutate(name = stringi::stri_trans_totitle(name)) %>%
  verify(dim(.) == c(314, 5)) %>%
  verify(has_all_names("prefcode", "meshcode", "name", "type", "geometry"))

# prefecture_mesh %>% count(meshcode, sort = TRUE)
# library(leaflet)
# pal <- colorFactor(palette = viridis::viridis(47),
#                    domain = stringr::str_pad(seq_len(47), width = 2, pad = "0"))
# leaflet(data = prefecture_mesh %>% filter(prefcode == "01")) %>% addTiles() %>%
#   addPolygons(label = ~prefcode,
#               color = ~pal(prefcode),
#               labelOptions = labelOptions(noHide = TRUE, textsize = "24px"))

usethis::use_data(prefecture_mesh, overwrite = TRUE)
