#################################################
# 都道府県データ
# Nippon::prefectures の改良
#################################################

options(digits = 9)
# Load Employed Packages --------------------------------------------------
library(rvest)
library(magrittr)
library(dplyr)

x <- read_html("https://ja.wikipedia.org/wiki/都道府県")
df <- x %>% html_table(fill = TRUE) %>%
  extract2(4)

df <- df[, c(2, 4, 6, 11)] %>%
  set_colnames(c("prefecture", "capital", "region", "jis_code")) %>%
  arrange(jis_code)

df %<>% mutate(jis_code = sprintf("%02d", jis_code),
              capital = recode(capital,
                               `東京（新宿区）` = "新宿区"))

x <- read_html("https://en.wikipedia.org/wiki/Prefectures_of_Japan")
df.en <- x %>% html_table(fill = TRUE) %>%
  extract2(4)
df.en <- df.en[, c(2, 5)] %>%
  set_colnames(c("prefecture", "major_island")) %>%
  mutate(major_island = recode(major_island,
                               `Hokkaido`       = "北海道",
                               `Honshu`         = "本州",
                               `Shikoku`        = "四国",
                               `Kyushu`         = "九州",
                               `Ryukyu Islands` = "琉球"))

jpnprefs <- df %>% left_join(df.en, by = "prefecture")

# get coordinates
library(purrr)
library(tidyr)
x <- read_html("https://ja.wikipedia.org/wiki/都道府県庁所在地")

l <- x %>% html_nodes(css = 'table') %>%
  extract(2) %>%
  html_nodes(css = 'tr > td > span.plainlinks > a') %>%
  html_attr("href") %>%
  map_chr(gsub, pattern = ".+params=", replacement = "") %>%
  map_chr(gsub, pattern = "region.+", replacement = "") %>%
  map_chr(gsub, pattern = "_E_", replacement = "")
# data.frameに変換
df.coords <- l %>% map(strsplit, split = "_N.") %>%
  flatten() %>%
  as.data.frame() %>%
  t() %>%
  set_rownames(NULL) %>%
  set_colnames(c("lat", "lon")) %>%
  as.data.frame()

df.coords %<>%
  separate(col = lat, into = c(paste("lat", c("d", "m", "s"), sep = "_")), sep = "_") %>%
  separate(lon, c(paste("lon", c("d", "m", "s"), sep = "_")), "_") %>%
  rowwise() %>%
  mutate(latitude  = as.numeric(lat_d) + (as.numeric(lat_m) + as.numeric(lat_s) / 60) / 60,
         longitude = as.numeric(lon_d) + (as.numeric(lon_m) + as.numeric(lon_s) / 60) / 60) %>%
  ungroup() %>%
  select(latitude, longitude)

jpnprefs %<>% bind_cols(df.coords) %>%
  select(jis_code, prefecture, capital, region, major_island, capital_latitude = latitude, capital_longitude = longitude)

expect_named(jpnprefs, c("jis_code", "prefecture", "capital", "region", "major_island", "capital_latitude", "capital_longitude"))
expect_equal(dim(jpnprefs), c(47, 7))

# jis codeの順で水準を作る
jpnprefs$prefecture %<>% factor() %>% forcats::fct_relevel(jpnprefs$prefecture)

devtools::use_data(jpnprefs, overwrite = TRUE)
