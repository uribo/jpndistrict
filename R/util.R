#' Get prefecture code from jis code
#'
#' @param jis_code jis code
pref_code <- function(jis_code) {
  res <- sprintf("%02d", as.numeric(substr(jis_code, 1, 2)))
  return(res)
}

#' Collect administration office point datasets.
#'
#' @param path path to P34 shapefile (if already exist)
#' @import magrittr
#' @importFrom sf read_sf
collect_ksj_p34 <- function(path = NULL) {

  jis_code <- NULL
  code <- gsub(".+P34-14_|_GML|/", "", path)

  d <- sf::read_sf(
    paste0(path, "/", list.files(path, pattern = paste0(code, ".shp$"))),
    stringsAsFactors = FALSE,
    options = c(paste0("ENCODING=",
                       dplyr::if_else(tolower(Sys.info()[["sysname"]]) == "windows",
                                      "UTF8", "cp932")))
  ) %>% set_colnames(c("jis_code", "type", "name", "address", "geometry")) %>%
    dplyr::mutate(jis_code = as.factor(jis_code))

  return(d)

}

#' Bind city area polygons to prefecture polygon
#'
#' @param path path to N03 shapefile (if already exist)
#' @importFrom sf read_sf
bind_cityareas <- function(path = NULL) {

  pref.shp <- NULL

  pref.shp <- sf::read_sf(
    list.files(path, pattern = "shp$", full.names = TRUE),
    stringsAsFactors = FALSE,
    options = c(paste0("ENCODING=",
                       dplyr::if_else(tolower(Sys.info()[["sysname"]]) == "windows",
                                      "UTF8", "cp932"))))

  res <- raw_bind_cityareas(pref.shp)

  return(res)
}

#' Intermediate function
#'
#' @param pref.shp geojsonio object (prefecture shapefile)
#' @import dplyr
#' @import sf
#' @importFrom tibble as_data_frame
raw_bind_cityareas <- function(pref.shp) {

  pref_name <- jiscode <- geometry <- NULL

  res <- st_union(pref.shp) %>%
    tibble::as_data_frame() %>%
    mutate(pref_name = pref.shp$pref_name[1],
           jiscode  = as.numeric(substr(pref.shp$city_code[1], 1, 2))) %>%
    select(pref_name, jiscode, geometry)

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
                    method   = "auto",
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
#' @import dplyr
#' @import sf
#' @importFrom stringi stri_trim_both
#' @importFrom stringi stri_conv
collect_cityarea <- function(path = NULL) {

  . <- N03_001 <- N03_002 <- N03_003 <- N03_004 <- N03_007 <- tmp_var <- NULL
  pref_name <- city_name_ <- city_name <- city_name_full <- city_code <- NULL

  res <- sf::read_sf(list.files(path, pattern = "shp$", full.names = TRUE, recursive = TRUE),
                     stringsAsFactors = FALSE,
                     options = c(paste0("ENCODING=",
                                        dplyr::if_else(tolower(Sys.info()[["sysname"]]) == "windows",
                                                       "UTF8", "cp932")))
                     ) %>%
    mutate(tmp_var = if_else(is.na(N03_003), "", N03_003),
           city_name_full = stringi::stri_trim_both(gsub("NA", "", paste(tmp_var, N03_004)))) %>%
    rename(pref_name = N03_001,
           city_name_ = N03_003, city_name = N03_004, city_code = N03_007) %>%
    select(pref_name,
           city_name_, city_name, city_name_full, city_code) %>%
    mutate_at(.vars = vars(contains("name")), stringi::stri_conv, to = "UTF8") %>%
    sf::st_simplify(preserveTopology = FALSE, dTolerance = 0.001) %>%
    filter(!is.na(st_dimension(.)))

  return(res)
}


#' Intermediate function
#'
#' @param code prefecture code (JIS X 0402)
#' @param path path to P34 shapefile (if already exist)
#' @importFrom readr read_rds
#' @importFrom utils download.file
#' @importFrom utils unzip
read_ksj_p34 <- function(code = NULL, path = NULL) {

  if (missing(path)) {
    download.file <- unzip <- NULL

    df.dl.url <- read_rds(system.file("extdata/ksj_P34_index.rds", package = "jpndistrict"))

    if (is.null(path) & file.exists(paste(tempdir(), df.dl.url$dest_file[code], sep = "/")) == FALSE) {

      utils::download.file(df.dl.url$zipFileUrl[code],
                    destfile = paste(tempdir(), df.dl.url$dest_file[code], sep = "/"),
                    method = "auto")
      utils::unzip(zipfile = paste(tempdir(), df.dl.url$dest_file[code], sep = "/"),
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
