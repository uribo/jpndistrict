####################################
# 市区町村役場データ https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-P34.html
# Last Update: 2020-11-26
###################################
library(magrittr)
# Download files ------------------------------------------------------
df_dl_url <-
  tibble::tibble(
  areaCode = as.character(seq_len(47)),
  zipFileUrl = paste0("https://nlftp.mlit.go.jp/ksj/gml/data/P34/P34-14/P34-14_",
                      stringr::str_pad(areaCode, width = 2, pad = "0"),
                      "_GML.zip"),
  dest_file = basename(zipFileUrl)) %>%
  assertr::verify(dim(.) == c(47, 3))

readr::write_rds(df_dl_url,
                 "inst/extdata/ksj_P34_index.rds")
