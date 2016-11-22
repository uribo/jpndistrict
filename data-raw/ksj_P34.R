library(kokudosuuchi)
library(spdplyr)
library(dplyr)

# Download shape files ------------------------------------------------------
# 行政区域 http://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-P34.html
df.dl.url <- kokudosuuchi::getKSJURL(identifier = "P34") %>%
  dplyr::mutate(dest_file = gsub("http://nlftp.mlit.go.jp/ksj/gml/data/P34/P34-14/", "", zipFileUrl)) %>%
  dplyr::select(identifier, year, areaCode, zipFileUrl, dest_file)

readr::write_rds(df.ksj34.dl.url, "inst/extdata/ksj_P34_index.rds")
