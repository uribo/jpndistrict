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
# dplyr # 0.7.0
# tidyr # 0.6.3
# purrr # 0.2.2.9000

# 都道府県
x <- read_html("https://ja.wikipedia.org/wiki/%E9%83%BD%E9%81%93%E5%BA%9C%E7%9C%8C")
df <- x %>% html_table(fill = TRUE) %>%
  extract2(4)

df <- df[, c(2, 4, 6, 11)] %>%
  set_colnames(c("prefecture", "capital", "region", "jis_code")) %>%
  arrange(jis_code) %>%
  mutate(jis_code = sprintf("%02d", jis_code),
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
# 都道府県庁所在地
x <- read_html("https://ja.wikipedia.org/wiki/%E9%83%BD%E9%81%93%E5%BA%9C%E7%9C%8C%E5%BA%81%E6%89%80%E5%9C%A8%E5%9C%B0")

l <- x %>% html_nodes(css = 'table') %>%
  magrittr::extract(2) %>%
  html_nodes(css = 'tr > td > span.plainlinks > a') %>%
  html_attr("href") %>%
  map_chr(gsub, pattern = ".+params=|region.+|_E_", replacement = "")

# data.frameに変換
df.coords <- l %>% map(strsplit, split = "_N.") %>%
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

jpnprefs %<>% bind_cols(df.coords) %>%
  # jis codeの順で水準を作る
  mutate(prefecture = prefecture %>% factor() %>% forcats::fct_relevel(prefecture)) %>%
  select(jis_code, prefecture, capital, region, major_island, capital_latitude = latitude, capital_longitude = longitude) %>%
  as_tibble()

expect_named(jpnprefs, c("jis_code", "prefecture", "capital", "region", "major_island", "capital_latitude", "capital_longitude"))
expect_equal(dim(jpnprefs), c(47, 7))
expect_s3_class(jpnprefs, c("data.frame", "tbl_df"))

devtools::use_data(jpnprefs, overwrite = TRUE)
