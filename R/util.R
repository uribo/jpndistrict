#' Get prefecture code from jis code
#'
#' @param jis_code jis code
pref_code <- function(jis_code) {
  res <- sprintf("%02d", as.numeric(substr(jis_code, 1, 2)))
  return(res)
}

#' Collect administration office point datasets.
#'
#' @param path path to N03 shapefile (if already exist)
#' @import dplyr
#' @import magrittr
#' @importFrom geojsonio geojson_read
#' @importFrom stringi stri_trim_both
collect_ksj_p34 <- function(path = NULL) {

  address <- NULL

  code <- gsub(".+P34-14_|_GML|/", "", path)

  d <- geojsonio::geojson_read(
    paste0(path, "/", list.files(path, pattern = paste0(code, ".shp$"))),
    method           = "local",
    what             = "sp",
    stringsAsFactors = TRUE,
    encoding         = "cp932")

  d@data <- d@data %>%
    bind_cols(as.data.frame(d@coords)) %>%
    set_colnames(c("jis_code", "type", "name", "address", "longitude", "latitude")) %>%
    rowwise() %>%
    mutate(address = stringi::stri_trim_both(address)) %>%
    ungroup()

  return(d)

}

#' Bind city area polygons to prefecture polygon
#'
#' @param path path to N03 shapefile (if already exist)
#' @importFrom geojsonio geojson_read
bind_cityareas <- function(path = NULL) {

  pref.shp <- NULL

  pref.shp <- geojsonio::geojson_read(list.files(path, pattern = "shp$", full.names = TRUE),
                                      method           = "local",
                                      what             = "sp",
                                      stringsAsFactors = TRUE)
  res <- raw_bind_cityareas(pref.shp)

  return(res)
}

#' Intermediate function
#'
#' @param pref.shp geojsonio object (prefecture shapefile)
#' @import spdplyr
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom rmapshaper ms_dissolve
raw_bind_cityareas <- function(pref.shp) {
  rmapshaperid <- NULL

  d.merge <- rmapshaper::ms_dissolve(pref.shp)

  res <- d.merge %>%
    mutate(pref_name = pref.shp$pref_name[1],
           jiscode  = as.numeric(substr(pref.shp$city_code[1], 1, 2))) %>%
    select(-rmapshaperid)

  return(res)
}

#' Intermediate function
#'
#' @description Download N03 raw data files or loading if file exists.
#' @param code prefecture code (JIS X 0402)
#' @param path path to N03 shapefile (if already exist)
read_ksj_cityarea <- function(code = NULL, path = NULL) {

  if (missing(path)) {
    path <- path_ksj_cityarea(code)
  }

  res <- collect_cityarea(path)

  return(res)

}

#' Download KSJ N03 zip files
#'
#' @param code prefecture code (JIS X 0402)
#' @param path path to N03 shapefile (if already exist)
#' @importFrom utils download.file
#' @importFrom utils unzip
path_ksj_cityarea <- function(code = NULL, path = NULL) {

  if (missing(path)) {

    pref.identifer <- sprintf("%02d", code)
    dest.path <- paste(tempdir(), paste0("N03-150101_", pref.identifer, "_GML.zip"), sep = "/")

    extract.path <- paste(tempdir(), pref.identifer,  sep = "/")

    # ksj zip file none
    if (is.null(path) & file.exists(dest.path) == FALSE) {

      utils::download.file(paste0("http://nlftp.mlit.go.jp/ksj/gml/data/N03/N03-15/N03-150101_", pref.identifer, "_GML.zip"),
                    destfile = dest.path,
                    method   = "wget",
                    quiet    = TRUE)
      utils::unzip(zipfile = dest.path,
                   exdir   = extract.path)

      path = paste(extract.path, gsub(".zip$", "", paste0("N03-20150101_", pref.identifer, "_GML.zip")), sep = "/")

      # ksj zip file exist
    } else if (file.exists(dest.path) == TRUE) {
      path = extract.path
    }

  }

  return(path)

}

#' Get prefecture code (JIS X 0402)
#'
#' @description Get prefecture code from prefecture of name or number.
#' @param code numeric
#' @param admin_name prefecture code for Japanese (character)
#' @import magrittr
#' @importFrom dplyr filter
#' @importFrom readr read_rds
collect_prefcode <- function(code = NULL, admin_name = NULL) {

  jis_code <- NULL
  jpnprefs <- read_rds(system.file(paste0("extdata/jpnprefs.rds"), package = "jpndistrict"))

  if (missing(admin_name)) {
    pref.code <- filter_(jpnprefs, ~jis_code == pref_code(code)) %>% magrittr::use_series(jis_code)
  } else if (missing(code)) {
    pref.code <- filter_(jpnprefs, ~prefecture == admin_name) %>% magrittr::use_series(jis_code)
  }

  return(pref.code)
}


#' Collect administration area
#'
#' @param path path to N03 shapefile (if already exist)
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom geojsonio geojson_read
#' @importFrom stringi stri_trim_both
collect_cityarea <- function(path = NULL) {

  N03_001 <- N03_002 <- N03_003 <- N03_004 <- N03_007 <- tmp_var <- NULL
  pref_name <- city_name_ <- city_name <- city_name_full <- city_code <- NULL

  res <- geojsonio::geojson_read(list.files(path, pattern = "shp$", full.names = TRUE, recursive = TRUE),
                          method           = "local",
                          what             = "sp",
                          stringsAsFactors = TRUE) %>%
    select(-N03_002) %>%
    mutate(N03_001 = as.character(N03_001),
           N03_003 = as.character(N03_003),
           N03_004 = as.character(N03_004),
           tmp_var = ifelse(is.na(N03_003), "", N03_003),
           city_name_full = stringi::stri_trim_both(gsub("NA", "", paste(tmp_var, N03_004)))) %>%
    rename(pref_name = N03_001, city_name_ = N03_003, city_name = N03_004, city_code = N03_007) %>%
    select(pref_name, city_name_, city_name, city_name_full, city_code)

  return(res)
}


#' Intermediate function
#'
#' @param code prefecture code (JIS X 0402)
#' @param path path to N03 shapefile (if already exist)
#' @importFrom readr read_rds
read_ksj_p34 <- function(code = NULL, path = NULL) {

  if (missing(path)) {
    download.file <- unzip <- NULL

    df.dl.url <- read_rds(system.file("extdata/ksj_P34_index.rds", package = "jpndistrict"))

    if (is.null(path) & file.exists(paste(tempdir(), df.dl.url$dest_file[code], sep = "/")) == FALSE) {

      download.file(df.dl.url$zipFileUrl[code],
                    destfile = paste(tempdir(), df.dl.url$dest_file[code], sep = "/"),
                    method = "wget")
      unzip(zipfile = paste(tempdir(), df.dl.url$dest_file[code], sep = "/"),
            exdir   = paste(tempdir(), gsub(".zip", "", df.dl.url$dest_file[code]),  sep = "/"))

      path = paste(tempdir(), gsub(".zip", "", df.dl.url$dest_file[code]),  sep = "/")
    } else if (file.exists(paste(tempdir(), df.dl.url$dest_file[code], sep = "/")) == TRUE) {
      path = paste(tempdir(), gsub(".zip", "", df.dl.url$dest_file[code]),  sep = "/")
    }

    res <- collect_ksj_p34(path = path)
  } else {
    res <- collect_ksj_p34(path = path)
  }

  return(res)
}
