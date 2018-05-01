# Load Employed Packages --------------------------------------------------
# 5MB以下
library(dplyr)
library(purrr)
library(magrittr)
library(testthat)
library(sf)
# Download raw data (47 prefectures, 2017) ----------------------------------------------------------------------
if (file.exists("data-raw/KSJ_N03/N03-170101_GML.zip") == FALSE) {

  dir.create("data-raw/KSJ_N03")

  d <- kokudosuuchi::getKSJURL(identifier = "N03")
  dl_url <- d %>%
    arrange(desc(year), areaCode) %>%
    slice(1L) %>%
    use_series(zipFileUrl)

  download.file(dl_url,
                paste0("data-raw/KSJ_N03/", basename(dl_url)))
  unzip(paste0("data-raw/KSJ_N03/", basename(dl_url)), exdir = "data-raw/KSJ_N03")
  # add to Git Ignore
}


# Modified shapefile ----------------------------------------------------------
sf_japan <- st_read("data-raw/KSJ_N03/N03-17_170101.shp") %>%
  set_names(c("prefecture", "sichyo_sinkyokyoku", "gun_seireishitei", "city", "city_code", "geometry")) %>%
  mutate(city = if_else(!is.na(gun_seireishitei),
                        paste(gun_seireishitei, city),
                        as.character(city)),
         pref_code = substr(city_code, 1, 2),
         city_code = as.character(city_code)) %>%
  select(pref_code, prefecture, sichyo_sinkyokyoku, city_code, city, geometry) %>%
  st_simplify(preserveTopology = TRUE, dTolerance = 0.0005) %>%
  st_transform(crs = 4326)
expect_gte(pryr::object_size(sf_japan), 57.5) # MB

# 市区町村で一つのPOLYGON、すなわちMULTIPOLYGONにする
sf_japan_distinct <- rbind(
  sf_japan %>%
    filter(city_code != "46527") %>%
    split(.$pref_code) %>%
    map(~ .x %>% distinct(city_code, .keep_all = TRUE)) %>%
    reduce(rbind),
  sf_japan %>%
    filter(city_code == "46527") %>%
    split(.$pref_code) %>%
    map(~ .x %>% distinct(city_code, .keep_all = TRUE)) %>%
    reduce(rbind)
)

expect_lte(pryr::object_size(sf_japan_distinct), 4831968) # MB

city_union <- function(df) {
  df %>%
    split(.$city_code) %>%
    map(~ st_buffer(st_union(st_buffer(., 0)), dist = 0.0001)) %>%
    reduce(c) %>%
    st_sfc()
}

tmp_a <- sf_japan %>%
  filter(city_code == "46527") %>%
  group_by(city_code) %>%
  do(out = st_buffer(st_union(st_buffer(., 0)), dist = 0.0001)) %>%
  use_series(out) %>%
  purrr::reduce(c) %>%
  st_sfc()

tmp_b <- sf_japan %>%
  filter(city_code == "46527") %>%
  city_union()
expect_true(identical(tmp_a, tmp_b))

# ちょい時間かかる (~15min.)
sf_japan_distinct$geometry <- c(sf_japan %>%
                                  filter(city_code != "46527") %>%
                                  city_union(),
                                sf_japan %>%
                                  filter(city_code == "46527") %>%
                                  city_union())

# 元に戻す
# 連鎖で実行しない (Errorになる)
sf_japan_distinct %<>%
  arrange(city_code)

sf_japan_distinct %>%
  filter(city == "倉敷市") %>%
  plot()
sf_japan %>%
  filter(city == "倉敷市") %>%
  plot()
sf_japan_distinct %>%
  filter(city == "岡山市 北区") %>%
  plot()
sf_japan_distinct %>%
  filter(grepl("岡山市", city)) %>%
  plot()

expect_equal(n_distinct(sf_japan_distinct$pref_code), 47L)

pref_modified <- function(prefcode) {
  # st_simplify でwarningが出るのでsuppress
  pref <- rlang::enquo(prefcode)

  res <- suppressWarnings(
    sf_japan_distinct %>%
      filter(pref_code == !!pref) %>%
      mutate_at(vars(c("prefecture", "sichyo_sinkyokyoku", "city")), stringi::stri_conv, to = "UTF8") %>%
      st_simplify(preserveTopology = TRUE, dTolerance = 0.0015) %>%
      filter(sf::st_is_empty(.) == FALSE)
  )
  return(res)
}


expect_equal(nrow(pref_modified(prefcode = 33)), 30L)
expect_equal(nrow(pref_modified(prefcode = 13)), 62L)

dir.create("inst/extdata/ksj_n03/")

# 5MB以内に収める
1:47 %>% purrr::walk(
  ~ sprintf("%02d", .x) %>% pref_modified(prefcode = .) %>%
    readr::write_rds(path = paste0("inst/extdata/ksj_n03/pref_", sprintf("%02s", .x), ".rds"),
                     compress = "xz")
)
