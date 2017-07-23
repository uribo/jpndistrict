# Load Employed Packages --------------------------------------------------
# 5MB以下
library(testthat)
library(dplyr)
library(magrittr)
library(sf) # 0.5-2

# Download shape files ------------------------------------------------------
if (file.exists("~/Dropbox/maps/shapefile/国土数値情報/N03/N03-20150101_GML/") == FALSE) {
  library(kokudosuuchi) # 0.2.0
  # 行政区域 http://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N03.html
  ids <- getKSJSummary() %>% filter(grepl("行政区域", title)) %>% use_series(identifier)
  # 全国を選択
  # 平成27年1月1日時点のデータ は条件を満たすことで二次利用が可能
  dl.url <- getKSJURL(identifier = ids) %>% filter(areaCode == 0, year == 2015) %>% use_series(zipFileUrl)

  expect_equal(dl.url,
               'http://nlftp.mlit.go.jp/ksj/gml/data/N03/N03-15/N03-150101_GML.zip')

  download.file(dl.url,
                destfile = '~/Dropbox/maps/shapefile/国土数値情報/N03/N03-150101_GML.zip')
  unzip(
    '~/Dropbox/maps/shapefile/国土数値情報/N03/N03-150101_GML.zip',
    exdir = "~/Dropbox/maps/shapefile/国土数値情報/N03"
  )
}

# 日本全体（項目の確認） -------------------------------------------------------------
jpn_pref_shp <- read_sf('~/Dropbox/maps/shapefile/国土数値情報/N03/N03-20150101_GML/',
                        stringsAsFactors = FALSE)
testthat::expect_s3_class(jpn_pref_shp, c('sf', 'data.frame'))
# 都道府県名, 支庁・振興局名(当該都道府県が「北海道」の場合、該当する支庁・振興局の名称), 郡・政令都市名, 市区町村名, 行政区域コード
testthat::expect_named(jpn_pref_shp, c('N03_001', 'N03_002', 'N03_003', 'N03_004', 'N03_007', 'geometry'))

# jpn_pref_shp %<>% #
#   # select(-N03_002) %>%
#   mutate(tmp_var = if_else(is.na(N03_003), "", N03_003),
#          city_name_full = stringr::str_trim(gsub("NA", "", paste(tmp_var, N03_004)))) %>%
#   rename(pref_name = N03_001,
#          # sicho_shinkou_name = N03_002,
#          city_name_ = N03_003, city_name = N03_004, city_code = N03_007) %>%
#   select(pref_name,
#          # sicho_shinkou_name,
#          city_name_, city_name, city_name_full, city_code) %>%
# 全国だから因子にするけど都道府県個別では不要
#   mutate(pref_name = pref_name %>% factor() %>% forcats::fct_inorder()) %>%
#   st_simplify(preserveTopology = TRUE, dTolerance = 0.020)

# 各都道府県のshapeからrdsを作成 -----------------------------------------------------
sf_pref_shp <- function(pref_num) {
  res <- read_sf(paste0("~/Dropbox/maps/shapefile/国土数値情報/N03/N03-20150101_", sprintf("%02d", pref_num),
                 "_GML/N03-15_", sprintf("%02d", pref_num), "_150101.shp"),
          stringsAsFactors = FALSE)

  return(res)
}

# sf_pref_shp(pref_num = 1)
# sf_pref_shp(pref_num = 13)

pref_modified <- function(...) {
  # st_simplify でwarningが出るのでsuppress
  res <- suppressWarnings(
    sf_pref_shp(pref_num = ...) %>%
    mutate(tmp_var = if_else(is.na(N03_003), "", N03_003),
           city_name_full = stringi::stri_trim_both(gsub("NA", "", paste(tmp_var, N03_004)))) %>%
    rename(pref_name = N03_001,
           # sicho_shinkou_name = N03_002,
           city_name_ = N03_003, city_name = N03_004, city_code = N03_007) %>%
    select(pref_name,
           # sicho_shinkou_name,
           city_name_, city_name, city_name_full, city_code) %>%
    mutate_at(.vars = vars(contains("name")), stringi::stri_conv, to = "UTF8") %>%
    st_simplify(preserveTopology = FALSE, dTolerance = 0.001) %>%
    filter(!is.na(st_dimension(.)))
    )
  return(res)
}

# pref_modified(pref_num = 13) %>%
#   select(city_name) %>%
#   plot()

1:47 %>% purrr::map(
  ~ pref_modified(pref_num = .x) %>% readr::write_rds(path = paste0("inst/extdata/pref_", sprintf("%02s", .x), "_city_spdf.rds"),
                                                      compress = "xz")
)
