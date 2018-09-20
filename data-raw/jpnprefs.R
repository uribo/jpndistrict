#################################################
# 都道府県データ
# Nippon::prefectures の改良
#################################################

options(digits = 9)
# Load Employed Packages --------------------------------------------------
library(rvest) # 0.3.2
library(magrittr) # 1.5
library(testthat) # 1.0.2
# forcats (0.2.0), devtools (1.13.2)
library(tidyverse)
# readr # 1.1.1
# dplyr # 0.7.6
# tidyr # 0.8.1
# purrr # 0.2.5

# Japanese ----------------------------------------------------------------
x <-
  read_html("https://ja.wikipedia.org/wiki/%E9%83%BD%E9%81%93%E5%BA%9C%E7%9C%8C")

df <-
  x %>%
  html_nodes(css = "table.wikitable:nth-child(104)") %>% # css to correct table as wiki page was edited
  html_table(fill = TRUE) %>%
  purrr::flatten_df() %>%
  select(2, 4, 6, 11) %>%
  set_colnames(c("prefecture", "capital", "region", "jis_code")) %>%
  arrange(jis_code) %>%
  mutate(jis_code = sprintf("%02d", jis_code),
         capital = recode(capital,
                          `東京（新宿区）` = "新宿区"))


# Roman -------------------------------------------------------------------
x <-
  read_html("https://en.wikipedia.org/wiki/Prefectures_of_Japan")

df_en <-
  x %>%
  html_nodes(css = "#mw-content-text > div > table.wikitable.sortable") %>%
  html_table(fill = TRUE) %>%
  purrr::flatten_df() %>%
  select(2, 5) %>%
  set_colnames(c("prefecture", "major_island")) %>%
  mutate(major_island = recode(major_island,
                               `Hokkaido`       = "北海道",
                               `Honshu`         = "本州",
                               `Shikoku`        = "四国",
                               `Kyushu`         = "九州",
                               `Ryukyu Islands` = "琉球"))

jpnprefs <-
  df %>%
  left_join(df_en, by = "prefecture")
rm(df, df_en)

# 都道府県庁所在地 ----------------------------------------------------------------
# get coordinates
x <-
  read_html("https://ja.wikipedia.org/wiki/%E9%83%BD%E9%81%93%E5%BA%9C%E7%9C%8C%E5%BA%81%E6%89%80%E5%9C%A8%E5%9C%B0")

l <-
  x %>%
  html_nodes(css = "table") %>%
  magrittr::extract(2) %>%
  html_nodes(css = "tr > td > span.plainlinks > a") %>%
  html_attr("href") %>%
  map_chr(gsub, pattern = ".+params=|region.+|_E_", replacement = "")

# data.frameに変換
df_coords <-
  l %>%
  map(strsplit, split = "_N.") %>%
  flatten() %>%
  as.data.frame() %>%
  t() %>%
  set_rownames(NULL) %>%
  set_colnames(c("lat", "lon")) %>%
  as.data.frame() %>%
  separate(col = lat, into = c(paste("lat", c("d", "m", "s"), sep = "_")), sep = "_") %>%
  separate(lon, c(paste("lon", c("d", "m", "s"), sep = "_")), "_") %>%
  mutate(latitude  = as.numeric(lat_d) + (as.numeric(lat_m) + as.numeric(lat_s) / 60) / 60,
         longitude = as.numeric(lon_d) + (as.numeric(lon_m) + as.numeric(lon_s) / 60) / 60) %>%
  select(latitude, longitude)

jpnprefs %<>%
  bind_cols(df_coords) %>%
  # jis codeの順で水準を作る
  mutate(prefecture = prefecture %>% factor() %>% forcats::fct_relevel(prefecture)) %>%
  select(jis_code, prefecture, capital, region, major_island, capital_latitude = latitude, capital_longitude = longitude) %>%
  as_tibble()

# ---- English region and island names
url <- "https://en.wikipedia.org/wiki/Prefectures_of_Japan"

jpn_pref_raw <- read_html(url) %>%
  html_nodes("table.wikitable:nth-child(49)") %>%
  html_table() %>%
  purrr::flatten_df()

jpn_pref_df <- jpn_pref_raw %>%
  select(2, 4, 5) %>%
  set_colnames(c("kanji", "region_en", "major_island_en")) %>%
  mutate(region_en = region_en %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT"))

# ---- English prefecture and capital names
url2 <- "https://en.wikipedia.org/wiki/List_of_Japanese_prefectures_by_population"

jpn_pref2_raw <- read_html(url2) %>%
  html_nodes("table.wikitable:nth-child(7)") %>%
  html_table() %>%
  purrr::flatten_df()

jpn_pref2_df <- jpn_pref2_raw %>%
  select(3, 2, 4) %>%
  set_colnames(c("kanji", "prefecture_en", "capital_en")) %>%
  mutate(prefecture_en = prefecture_en %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT"),
         capital_en = capital_en %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT"))

# ---- Join with jpnprefs
jpnprefs <- jpnprefs %>%
  left_join(jpn_pref_df, by = c("prefecture" = "kanji")) %>%
  left_join(jpn_pref2_df, by = c("prefecture" = "kanji")) %>%
  select(jis_code, prefecture, capital, region, major_island,
         prefecture_en, capital_en, region_en, major_island_en,
         capital_latitude, capital_longitude) %>%
  as_tibble()

expect_named(jpnprefs,
             c("jis_code", "prefecture", "capital", "region", "major_island",
               "prefecture_en", "capital_en", "region_en", "major_island_en",
               "capital_latitude", "capital_longitude"))
expect_equal(dim(jpnprefs),
             c(47, 11))
expect_s3_class(jpnprefs,
                c("data.frame", "tbl_df"))

devtools::use_data(jpnprefs, overwrite = TRUE)
readr::write_rds(jpnprefs, "inst/extdata/jpnprefs.rds")
