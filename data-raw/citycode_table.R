# remotes::install_github("uribo/odkitchen")
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(assertr)

if (dir.exists(here::here("data-raw/mic_city_table")) == FALSE)
  dir.create(here::here("data-raw/mic_city_table"))

if (file.exists(here::here("data-raw/mic_city_table/000562731.xls")) == FALSE) {
  base_url <- "http://www.soumu.go.jp/"
  x <-
    read_html(glue::glue(base_url, "denshijiti/code.html"))

  tgt_file <-
    x %>%
    html_nodes(css = '#contentsWrapper > div.contentsBody > div > div:nth-child(2) > ul > li:nth-child(2) > ul > li:nth-child(2) > a') %>%
    html_attr("href")

  glue::glue(base_url, tgt_file) %>%
    download.file(destfile = here::here("data-raw", "mic_city_table", basename(files)))
  rm(x)
} else {
  tgt_file <-
    list.files(here::here("data-raw", "mic_city_table"), pattern = "000562731.xls$", full.names = TRUE)
}

d_orig <-
  readxl::read_xls(tgt_file,
                   sheet = 1,
                   skip = 1) %>%
  verify(dim(.) == c(1438, 14)) %>%
  dplyr::select(5, 6, 7,
         9, 10, 11, 12, 14) %>%
  dplyr::slice(-seq_len(2)) %>%
  verify(dim(.) == c(1436, 8)) %>%
  purrr::set_names(
    c("prefecture"),
    paste0("before_", c("code", "city_name")),
    "type",
    "date",
    paste0("after_", c("code", "city_name")),
    "reason"
  )

d_mod <-
  d_orig %>%
  mutate_at(vars(c("type", "date", "after_code", "after_city_name")),
            .funs = list(~ if_else(. == "〃",
                         NA_character_,
                         .))) %>%
  mutate(id = if_else(!is.na(prefecture),
                      row_number(),
                      NA_integer_)) %>%
  fill(id, .direction = "down") %>%
  mutate(after_city_name = if_else(after_city_name == "同左",
                                   before_city_name,
                                   after_city_name),
         after_code = if_else(after_code == "同左",
                              before_code,
                              after_code)) %>%
  group_by(id) %>%
  tidyr::nest()

d_mod <-
  d_mod %>%
  mutate(data = purrr::map(data,
                           ~ .x %>%
                             mutate_at(vars(ends_with("_code")),
                                       funs(stringr::str_sub(., 1, 5))))) %>%
  mutate(data = purrr::map(data,
                           ~ .x %>%
                             mutate_at(vars(starts_with("after_")), funs(if_else(. == "削除",
                                                                                 NA_character_,
                                                                                 .))) %>%
                             arrange(after_code) %>%
                             fill(date, starts_with("after_"), .direction = "down") %>%
                             dplyr::select(date,
                                    before_code, before_city_name,
                                    after_code, after_city_name))) %>%
  mutate(data = purrr::map(data,
                           ~ .x %>%
                             arrange(before_code) %>%
                             fill(starts_with("before_"), .direction = "down"))) %>%
  tidyr::unnest(cols = data) %>%
  arrange(id, date) %>%
  verify(dim(.) == c(1436, 6)) %>%
  group_by(id) %>%
  tidyr::fill(date, .direction = "down") %>%
  ungroup() %>%
  verify(sum(is.na(.$date)) == 0)

citycode_sets <-
  d_mod %>%
  dplyr::filter(!stringr::str_detect(date, "^H")) %>%
  verify(nrow(.) == 1382) %>%
  mutate(
    date = if_else(!stringr::str_detect(date, "^H"),
                   as_date(as.numeric(date), origin = "1900-01-01") - days(2),
                   lubridate::as_date(date))) %>%
  bind_rows(
    d_mod %>%
      dplyr::filter(stringr::str_detect(date, "^H")) %>%
      verify(nrow(.) == 54) %>%
      mutate(date = purrr::map(date,
                               ~ .x %>%
                                 strsplit(split = "\\.") %>%
                                 purrr::flatten() %>%
                                 purrr::modify_at(1,
                                                  odkitchen::convert_jyr) %>%
                                 purrr::modify_at(c(2,3),
                                                  as.numeric) %>%
                                 purrr::flatten_dbl() %>%
                                 paste(collapse = "-") %>%
                                 lubridate::as_date())) %>%
      tidyr::unnest_longer(col = date)
  ) %>%
  arrange(date, after_code) %>%
  verify(dim(.) == c(1436, 6))

citycode_sets <-
  citycode_sets %>%
  mutate(.id = as.numeric(forcats::fct_inorder(as.character(id)))) %>%
  dplyr::select(-id) %>%
  dplyr::select(
    date,
    starts_with("before"),
    starts_with("after"),
    .id) %>%
  verify(dim(.) == c(1436, 6)) %>%
  verify(sum(is.na(.$before_code)) == 1L)

readr::write_rds(citycode_sets,
                 "inst/extdata/citycode_sets.rds",
                 compress = "xz")
