#################################################
# Nippon::prefectures
#################################################

options(digits = 9)
# Load Employed Packages --------------------------------------------------
library(rvest)
library(assertr)
library(dplyr)
library(purrr)

# Japanese ----------------------------------------------------------------
x <-
  enc2native(
      intToUtf8(c(26481, 20140, 65288, 26032, 23487, 21306, 65289),
                multiple = FALSE))
xx <-
  enc2native(intToUtf8(c(26032, 23487, 21306),
                             multiple = FALSE))

df_prefs_jp <-
  read_html("https://ja.wikipedia.org/wiki/%E9%83%BD%E9%81%93%E5%BA%9C%E7%9C%8C") %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[4]') %>% # css to correct table as wiki page was edited
  html_table(fill = TRUE) %>%
  flatten_df() %>%
  verify(dim(.) == c(47, 12)) %>%
  select(2, 4, 6, 11) %>%
  set_names(c("prefecture", "capital", "region", "jis_code")) %>%
  arrange(jis_code) %>%
  mutate(jis_code = sprintf("%02d", jis_code),
         capital = recode(capital,
                          x =
                            xx),
         prefecture = pmap_chr(.,
                                      ~ gsub(pattern = "[0-9]{2}", replacement =  "", x = ..1,))) %>%
  verify(dim(.) == c(47, 4))

# Roman -------------------------------------------------------------------
df_prefs_en <-
  read_html("https://en.wikipedia.org/wiki/Prefectures_of_Japan") %>%
  html_nodes(css = "#mw-content-text > div > table.wikitable.sortable") %>%
  html_table(fill = TRUE) %>%
  flatten_df() %>%
  verify(dim(.) == c(47, 12)) %>%
  select(2, 5) %>%
  set_names(c("prefecture", "major_island")) %>%
  mutate(major_island = recode(major_island,
                               `Hokkaido`       = enc2native(intToUtf8(c(21271, 28023, 36947),
                                                                       multiple = FALSE)),
                               `Honshu`         = enc2native(intToUtf8(c(26412, 24030),
                                                                       multiple = FALSE)),
                               `Shikoku`        = enc2native(intToUtf8(c(22235, 22269),
                                                                       multiple = FALSE)),
                               `Kyushu`         = enc2native(intToUtf8(c(20061, 24030),
                                                                       multiple = FALSE)),
                               `Ryukyu Islands` = enc2native(intToUtf8(c(29705, 29699),
                                                                       multiple = FALSE)))) %>%
  verify(dim(.) == c(47, 2))

jpnprefs <-
  df_prefs_jp %>%
  left_join(df_prefs_en, by = "prefecture")

# Adminitration Office Locaiton ----------------------------------------------------------------
# get coordinates
coords_string <-
  read_html("https://ja.wikipedia.org/wiki/%E9%83%BD%E9%81%93%E5%BA%9C%E7%9C%8C%E5%BA%81%E6%89%80%E5%9C%A8%E5%9C%B0") %>%
  html_nodes(css = "table") %>%
  magrittr::extract(2) %>%
  html_nodes(css = "tr > td > span.plainlinks > a") %>%
  html_attr("href") %>%
  map_chr(gsub, pattern = ".+params=|region.+|_E_", replacement = "")

df_coords <-
  coords_string %>%
  map(strsplit, split = "_N.") %>%
  flatten() %>%
  as.data.frame() %>%
  t() %>%
  as.data.frame() %>%
  tibble::remove_rownames() %>%
  set_names(c("lat", "lon")) %>%
  verify(dim(.) == c(47, 2)) %>%
  tidyr::separate(col = lat, into = c(paste("lat", c("d", "m", "s"), sep = "_")), sep = "_") %>%
  tidyr::separate(lon, c(paste("lon", c("d", "m", "s"), sep = "_")), "_") %>%
  verify(ncol(.) == 6) %>%
  mutate(latitude  = as.numeric(lat_d) + (as.numeric(lat_m) + as.numeric(lat_s) / 60) / 60,
         longitude = as.numeric(lon_d) + (as.numeric(lon_m) + as.numeric(lon_s) / 60) / 60) %>%
  select(latitude, longitude) %>%
  verify(dim(.) == c(47, 2))

jpnprefs <-
  jpnprefs %>%
  bind_cols(df_coords) %>%
  verify(ncol(.) == 7) %>%
  # jis code
  select(jis_code, prefecture,
         capital, region, major_island,
         capital_latitude = latitude, capital_longitude = longitude) %>%
  as_tibble() %>%
  verify(dim(.) == c(47, 7))

# ---- English region and island names
jpn_pref_raw <-
  read_html("https://en.wikipedia.org/wiki/Prefectures_of_Japan") %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[4]') %>%
  html_table() %>%
  flatten_df() %>%
  verify(dim(.) == c(47, 12))

jpn_pref_df <-
  jpn_pref_raw %>%
  select(2, 4, 5) %>%
  set_names(c("kanji", "region_en", "major_island_en"))

# ---- English prefecture and capital names
jpn_pref2_raw <-
  read_html("https://en.wikipedia.org/wiki/List_of_Japanese_prefectures_by_population") %>%
  html_nodes(css = "#mw-content-text > div > table:nth-child(8)") %>%
  html_table() %>%
  flatten_df() %>%
  filter(!is.na(Rank)) %>%
  verify(dim(.) == c(47, 10))

jpn_pref2_df <-
  jpn_pref2_raw %>%
  select(3, 2, 4) %>%
  set_names(c("kanji", "prefecture_en", "capital_en"))

# ---- Join with jpnprefs
jpnprefs <-
  jpnprefs %>%
  left_join(jpn_pref_df, by = c("prefecture" = "kanji")) %>%
  left_join(jpn_pref2_df, by = c("prefecture" = "kanji")) %>%
  select(jis_code, prefecture, capital, region, major_island,
         prefecture_en, capital_en, region_en, major_island_en,
         capital_latitude, capital_longitude) %>%
  as_tibble() %>%
  mutate(prefecture = prefecture %>% as.factor() %>% forcats::fct_relevel(prefecture),
         prefecture_en = prefecture_en %>% as.factor() %>% forcats::fct_relevel(prefecture_en)) %>%
  mutate_at(.vars = vars(ends_with("_en")),
            .funs = list(~ iconv(x = ., from = "UTF-8", to = "ASCII//TRANSLIT")))

usethis::use_data(jpnprefs, overwrite = TRUE)
