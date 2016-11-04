library(kokudosuuchi)
library(spdplyr)
library(dplyr)

# Download shape files ------------------------------------------------------
# 行政区域 http://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-P34.html
# 全国を選択

ddd <- function(code = NULL, path = NULL) {

  if (dir.exists(paste0(path, gsub(".zip", "", dl.url$dest_file[33]))) == FALSE) {

    dl.url <- kokudosuuchi::getKSJURL(identifier = "P34") %>%
      mutate(dest_file = gsub("http://nlftp.mlit.go.jp/ksj/gml/data/P34/P34-14/", "", zipFileUrl))

    download.file(dl.url$zipFileUrl[code],
                  destfile = paste0(path, dl.url$dest_file[code]),
                  method = "wget")
    unzip(zipfile = paste0(path, dl.url$dest_file[code]),
          exdir   = paste0(path, gsub(".zip", "", dl.url$dest_file[code])))
  }

  d <- geojsonio::geojson_read(
    paste0(path, gsub(".zip", "", dl.url$dest_file[code]), "/", gsub(".zip", "", dl.url$dest_file[code]) %>% gsub("_GML", "", .), ".shp"),
    method = "local",
    what = "sp",
    stringsAsFactors = TRUE,
    encoding = "cp932")

  d@data <- d@data %>%
    bind_cols(as.data.frame(d@coords)) %>%
    set_colnames(c("jis_code", "type", "name", "address", "longitude", "latitude")) %>%
    rowwise() %>%
    mutate(address = stringr::str_trim(address)) %>%
    ungroup()
  return(d)
}

d <- ddd(code = 47, path = "data-raw/shapefiles/ksj_P34/") %>% as.data.frame()

d %<>% filter(!grepl("(支所|出張所|庁舎)$", name))

library(plotly)

g <- list(
  scope = "asia",
  showland = TRUE,
  landcolor = toRGB("grey83"),
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white"),
  showlakes = TRUE,
  lakecolor = toRGB("white"),
  showsubunits = TRUE,
  showcountries = TRUE,
  resolution = 50,
  projection = list(
    type = "conic conformal",
    rotation = list(lon = 130)
  ),
  lonaxis = list(
    showgrid = TRUE,
    gridwidth = 0.5,
    range = c(min(d$longitude) - 3, max(d$longitude) + 3),
    dtick = 5
  ),
  lataxis = list(
    showgrid = TRUE,
    gridwidth = 0.5,
    range = c(min(d$latitude) - 5, max(d$latitude) + 5),
    dtick = 5
  )
)

plot_geo(d, lat = ~latitude, lon = ~longitude) %>%
  add_markers(
    text = ~paste(d$name, "<br>", d$address), hoverinfo = "text"
  ) %>%
  layout(geo = g)
