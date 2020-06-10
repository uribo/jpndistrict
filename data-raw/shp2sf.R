# Load Employed Packages --------------------------------------------------
# # Fit within 5MB
library(dplyr)
library(purrr)
library(testthat)
library(sf)
library(googlePolylines)
# Download raw data (47 prefectures, 2017) ----------------------------------------------------------------------
if (!file.exists("data-raw/KSJ_N03/N03-17_170101.shp")) {
  dir.create("data-raw/KSJ_N03")
  download.file("https://nlftp.mlit.go.jp/ksj/gml/data/N03/N03-2017/N03-170101_GML.zip",
                destfile = "data-raw/KSJ_N03/N03-170101_GML.zip")
  unzip("data-raw/KSJ_N03/N03-170101_GML.zip",
        exdir = "data-raw/KSJ_N03")
  unlink("data-raw/KSJ_N03/N03-170101_GML.zip")
  unlink("data-raw/KSJ_N03/N03-17_170101.xml")
  usethis::use_git_ignore("data-raw/KSJ_N03/")
}

# Modified shapefile ----------------------------------------------------------
sf_japan <-
  st_read("data-raw/KSJ_N03/N03-17_170101.shp",
          stringsAsFactors = FALSE,
          as_tibble = TRUE) %>%
  set_names(c("prefecture", "sichyo_sinkyokyoku", "gun_seireishitei",
              "city", "city_code", "geometry")) %>%
  mutate(city = if_else(!is.na(gun_seireishitei),
                        if_else(is.na(city),
                                gun_seireishitei,
                                paste(gun_seireishitei, city)),
                        as.character(city)),
         pref_code = substr(city_code, 1, 2),
         city_code = as.character(city_code)) %>%
  select(pref_code, prefecture, sichyo_sinkyokyoku,
         city_code, city, geometry) %>%
  st_transform(crs = 4326)

expect_gte(pryr::object_size(sf_japan), 273) # MB

# Set to MULTIPOLYGON when it consists of one POLYGON
# and multiple POLYGON in the city or ward
city_union <- function(df, prefcode_var, citycode_var, cityname_var) {
  prefcode_var <- rlang::enquo(prefcode_var)
  citycode_var <- rlang::enquo(citycode_var)
  cityname_var <- rlang::enquo(cityname_var)
  df %>%
    dplyr::filter(!is.na(!!citycode_var)) %>%
    dplyr::group_by(!!prefcode_var, !!citycode_var, !!cityname_var) %>%
    dplyr::group_map(
      ~ .x %>%
        sf::st_make_valid() %>%
        sf::st_union() %>%
        sf::st_buffer(dist = 0.0001) %>%
        st_simplify(preserveTopology = TRUE, dTolerance = 0.0015)
    ) %>%
    purrr::reduce(c) %>%
    sf::st_sfc()
}

# ~ 15min
sf_japan_distinct <-
  sf_japan %>%
  st_drop_geometry() %>%
  distinct(pref_code, prefecture, sichyo_sinkyokyoku, city_code, city) %>%
  assertr::verify(dim(.) == c(1909, 5)) %>%
  filter(!is.na(city_code)) %>%
  assertr::verify(nrow(.) == 1902L) %>%
  mutate_at(vars(c("prefecture", "sichyo_sinkyokyoku", "city")),
            stringi::stri_conv,
            to = "UTF8") %>%
  mutate(geometry = city_union(sf_japan, pref_code, city_code, city)) %>%
  st_as_sf() %>%
  st_make_valid() %>%
  arrange(city_code)
expect_equal(n_distinct(sf_japan_distinct$pref_code), 47L)

if (!dir.exists("inst/extdata/ksj_n03/"))
  dir.create("inst/extdata/ksj_n03/")

sprintf("%02d", seq_len(47)) %>%
  purrr::walk(
    ~ sf_japan_distinct %>%
      filter(pref_code == .x) %>%
      googlePolylines::encode() %>%
      readr::write_rds(path = paste0("inst/extdata/ksj_n03/pref_",
                                     .x, ".rds"),
                       compress = "xz"))

sprintf("%02d", seq_len(47)) %>%
  purrr::map(
    ~ file.size(paste0(
      "inst/extdata/ksj_n03/pref_",
      .x,
      ".rds"
    ))
  ) %>%
  purrr::reduce(`+`)
