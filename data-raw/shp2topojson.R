# Load Employed Packages --------------------------------------------------
library(testthat)
library(dplyr)
library(kokudosuuchi)
library(geojsonio)
library(rgeos)
library(spdplyr)
library(foreach)

# Download shape files ------------------------------------------------------
# 行政区域 http://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N03.html
ids <- getKSJSummary() %>% filter(grepl("行政区域", title)) %>% use_series(identifier)
# 全国を選択
# 平成27年4月1日時点のデータ は条件を満たすことで二次利用が可能
dl.url <- getKSJURL(identifier = ids) %>% filter(areaCode == 0, year == 2015) %>% use_series(zipFileUrl)

download.file(dl.url,
              destfile = "data-raw/shapefiles/N03-150101_GML.zip")

# Generate geojson -------------------------------------------------------
jpn_pref_shp <- geojsonio::geojson_read("data-raw/shapefiles/N03-20150101_GML/N03-15_150101.shp",
                        method           = "local",
                        what             = "sp",
                        stringsAsFactors = TRUE)
testthat::expect_s4_class(jpn_pref_shp, "SpatialPolygonsDataFrame")
# 都道府県名, 支庁・振興局名(当該都道府県が「北海道」の場合、該当する支庁・振興局の名称), 郡・政令都市名, 市区町村名, 行政区域コード
testthat::expect_named(jpn_pref_shp@data, c("N03_001", "N03_002", "N03_003", "N03_004", "N03_007"))

m <- jpn_pref_shp %>% fortify()

jppref <- unique(jpn_pref_shp@data$N03_001)

#都道府県ごとのrds(市区町村境界あり)
# 1行政単位のgeojsonを出力
foreach(i = 1:length(jppref)) %do% {
  jpn_pref_shp %>%
    filter(N03_001 == jppref[i]) %>%
    select(-N03_002) %>%
    mutate(N03_001 = as.character(N03_001),
           N03_003 = as.character(N03_003),
           N03_004 = as.character(N03_004),
           tmp_var = ifelse(is.na(N03_003), "", N03_003),
           city_name_full = stringr::str_trim(gsub("NA", "", paste(tmp_var, N03_004)))) %>%
    rename(pref_name = N03_001, city_name_ = N03_003, city_name = N03_004, city_code = N03_007) %>%
    select(pref_name, city_name_, city_name, city_name_full, city_code) %>%
    readr::write_rds(path = paste0("inst/extdata/pref_", sprintf("%02s", i), "_city_spdf.rds"))
}

# 都道府県ごとのrds(市区町村境界なし)
dist_geo_0 <- rgeos::gUnaryUnion(jpn_pref_shp, id = jpn_pref_shp@data$N03_001)

# jppref[jppref %in% dist_geo_0@polygons[[37]]@ID] # 13
# jppref[jppref %in% dist_geo_0@polygons[[37]]@ID] %>% as.numeric()
jppref.new.order <- unique(jpn_pref_shp@data$N03_001) %>% forcats::fct_relevel(unique(jpn_pref_shp@data$N03_001) %>% as.character())
#
# jppref.new.order[jppref.new.order %in% dist_geo_0@polygons[[1]]@ID] %>% as.numeric()
#
# jppref[jppref %in% dist_geo_0@polygons[[1]]@ID]

foreach(i = 1:length(jppref)) %do% {
  geojson_write(sp::SpatialPolygons(dist_geo_0@polygons[i]), lat = "lat", lon = "long", geometry = "polygon")

  tmp <- geojson_read("myfile.geojson", method = "local", what = "sp") %>%
    mutate(pref_name = dist_geo_0@polygons[[i]]@ID,
           jiscode = as.numeric(jppref.new.order[jppref.new.order %in% dist_geo_0@polygons[[i]]@ID])) %>%
    select(-dummy)

  readr::write_rds(tmp,
                   path = paste0("inst/extdata/pref_", sprintf("%02s", as.numeric(jppref.new.order[jppref.new.order %in% dist_geo_0@polygons[[i]]@ID])), "_spdf.rds"))
}

# 日本全体 --------------------------------------------------------------------
jpn_pref_shp@data %<>%
  mutate(dummy_id = 0)

# めちゃくしゃ時間がかかる...
jpn_composite <- rgeos::gUnaryUnion(jpn_pref_shp, id = jpn_pref_shp@data$dummy_id)

geojson_write(sp::SpatialPolygons(jpn_composite@polygons), lat = "lat", lon = "long", geometry = "polygon")

tmp <- geojson_read("myfile.geojson", method = "local", what = "sp") %>%
  mutate(pref_name = jpn_composite@polygons[[i]]@ID,
         jiscode = as.numeric(jppref.new.order[jppref.new.order %in% dist_geo_0@polygons[[i]]@ID])) %>%
  select(-dummy)


jp_map <- geojsonio::geojson_read("myfile.geojson",
                                  method = "local",
                                  what = "sp"
) %>% fortify()

ggplot() +
  geom_map(data = jp_map,
           map  = jp_map,
           aes(x = long, y = lat, map_id = id),
           fill = "white", color = "black",
           size = 0.5) +
  coord_map() +
  ggthemes::theme_map() %>%
  roracle::notice_status()
