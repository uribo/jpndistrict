library(dplyr)

# Download files ------------------------------------------------------
# 市区町村役場データ http://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-P34.html
df_dl_url <-
  kokudosuuchi::getKSJURL(identifier = "P34") %>%
  dplyr::mutate(dest_file = gsub("http://nlftp.mlit.go.jp/ksj/gml/data/P34/P34-14/", "", zipFileUrl)) %>%
  dplyr::select(identifier, year, areaCode, zipFileUrl, dest_file) %>%
  assertr::verify(dim(.) == c(47, 5))

readr::write_rds(df_dl_url,
                 "inst/extdata/ksj_P34_index.rds")
