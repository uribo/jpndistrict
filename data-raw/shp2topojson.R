# Load Employed Packages --------------------------------------------------
library(testthat)
library(dplyr)
library(kokudosuuchi)
library(geojsonio)
library(rgeos)
library(spdplyr)
library(rmapshaper)
library(foreach)

# Download shape files ------------------------------------------------------
# 行政区域 http://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N03.html
ids <- getKSJSummary() %>% filter(grepl("行政区域", title)) %>% use_series(identifier)
# 全国を選択
# 平成27年4月1日時点のデータ は条件を満たすことで二次利用が可能
dl.url <- getKSJURL(identifier = ids) %>% filter(areaCode == 0, year == 2015) %>% use_series(zipFileUrl)

download.file(dl.url,
              destfile = "~/Dropbox/maps/shapefile/国土数値情報/N03/N03-150101_GML.zip")

# Generate geojson -------------------------------------------------------
jpn_pref_shp <- geojsonio::geojson_read("~/Dropbox/maps/shapefile/国土数値情報/N03/N03-20150101_37_GML/N03-15_37_150101.shp",
                        method           = "local",
                        what             = "sp",
                        stringsAsFactors = TRUE)
testthat::expect_s4_class(jpn_pref_shp, "SpatialPolygonsDataFrame")
# 都道府県名, 支庁・振興局名(当該都道府県が「北海道」の場合、該当する支庁・振興局の名称), 郡・政令都市名, 市区町村名, 行政区域コード
testthat::expect_named(jpn_pref_shp@data, c("N03_001", "N03_002", "N03_003", "N03_004", "N03_007"))

jppref <- unique(jpn_pref_shp@data$N03_001)

# 全国のデータを都道府県ごとのrdsに分割(市区町村境界あり)
# 1行政単位のgeojsonを出力
foreach(i = 1:47, .errorhandling = "pass") %do% {
  geojsonio::geojson_read(paste0("~/Dropbox/maps/shapefile/国土数値情報/N03/N03-20150101_", sprintf("%02d", i),
                                 "_GML/N03-15_", sprintf("%02d", i), "_150101.shp"),
                          method           = "local",
                          what             = "sp",
                          stringsAsFactors = TRUE) %>%
    ms_simplify(keep = 0.030, method = "dp") %>%
    select(-N03_002) %>%
    mutate(N03_001 = as.character(N03_001),
           N03_003 = as.character(N03_003),
           N03_004 = as.character(N03_004),
           tmp_var = ifelse(is.na(N03_003), "", N03_003),
           city_name_full = stringr::str_trim(gsub("NA", "", paste(tmp_var, N03_004)))) %>%
    rename(pref_name = N03_001, city_name_ = N03_003, city_name = N03_004, city_code = N03_007) %>%
    select(pref_name, city_name_, city_name, city_name_full, city_code) %>%
    readr::write_rds(path = paste0("inst/extdata/pref_", sprintf("%02s", i), "_city_spdf.rds"), compress = "xz")
}

