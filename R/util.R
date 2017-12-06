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
#' @importFrom magrittr extract
#' @importFrom sf st_read
#' @importFrom dplyr mutate if_else
collect_ksj_p34 <- function(path = NULL) {

  jis_code <- NULL
  code <- gsub(".+P34-14_|_GML|/", "", path)

  d <- sf::st_read(
    paste0(path, "/", list.files(path, pattern = paste0(code, ".shp$"))),
    stringsAsFactors = FALSE,
    options = c(paste0("ENCODING=",
                       dplyr::if_else(tolower(Sys.info()[["sysname"]]) == "windows",
                                      "UTF8", "cp932")))
  ) %>% magrittr::set_colnames(c("jis_code", "type", "name", "address", "geometry")) %>%
    dplyr::mutate(jis_code = as.factor(jis_code))

  return(d)

}

#' Bind city area polygons to prefecture polygon
#'
#' @param path path to N03 shapefile (if already exist)
#' @importFrom sf st_read
bind_cityareas <- function(path = NULL) {

  pref.shp <- NULL

  pref.shp <- sf::st_read(
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
#' @param pref sf object (prefecture)
#' @importFrom dplyr mutate
#' @importFrom sf st_buffer st_sf st_union
raw_bind_cityareas <- function(pref) {

  prefecture <- city_code <- geometry <- NULL

  res <- suppressMessages(suppressWarnings(sf::st_buffer(pref, 0) %>%
                                              sf::st_union() %>%
    sf::st_sf() %>%
    dplyr::mutate(jis_code  = as.numeric(substr(pref$city_code[1], 1, 2)),
                  prefecture = pref$prefecture[1]) %>%
    sf::st_buffer(dist = 0.001)))

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
#' @importFrom magrittr use_series
#' @importFrom dplyr filter
collect_prefcode <- function(code = NULL, admin_name = NULL) {

  jis_code <- prefecture <- NULL

  if (missing(admin_name)) {
    pref_code <- dplyr::filter(jpnprefs, jis_code == pref_code(code)) %>% magrittr::use_series(jis_code)
  } else if (missing(code)) {
    pref_code <- dplyr::filter(jpnprefs, prefecture == admin_name) %>% magrittr::use_series(jis_code)
  }

  return(pref_code)
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

  res <- sf::st_read(list.files(path, pattern = "shp$", full.names = TRUE, recursive = TRUE),
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
#' @param pref_code prefecture code (JIS X 0402)
#' @param path path to P34 shapefile (if already exist)
#' @importFrom readr read_rds
#' @importFrom utils download.file
#' @importFrom utils unzip
read_ksj_p34 <- function(pref_code = NULL, path = NULL) {

  if (missing(path)) {
    download.file <- unzip <- NULL

    df.dl.url <- readr::read_rds(system.file("extdata/ksj_P34_index.rds", package = "jpndistrict"))

    if (is.null(path) & file.exists(paste(tempdir(), df.dl.url$dest_file[pref_code], sep = "/")) == FALSE) {

      utils::download.file(df.dl.url$zipFileUrl[pref_code],
                    destfile = paste(tempdir(), df.dl.url$dest_file[pref_code], sep = "/"),
                    method = "auto")
      utils::unzip(zipfile = paste(tempdir(), df.dl.url$dest_file[pref_code], sep = "/"),
            exdir   = paste(tempdir(), gsub(".zip", "", df.dl.url$dest_file[pref_code]),  sep = "/"))

      path = paste(tempdir(), gsub(".zip", "", df.dl.url$dest_file[pref_code]),  sep = "/")
    } else if (file.exists(paste(tempdir(), df.dl.url$dest_file[pref_code], sep = "/")) == TRUE) {
      path = paste(tempdir(), gsub(".zip", "", df.dl.url$dest_file[pref_code]),  sep = "/")
    }

    res <- collect_ksj_p34(path = path)
  } else {
    res <- collect_ksj_p34(path = path)
  }

  return(res)
}

#' Internal function
#'
#' @param longitude longitude
#' @param latitude latitude
#' @param ... export parameter to other functions
#' @importFrom purrr map reduce
#' @importFrom sf st_contains st_point
#' @name which_pol_min
which_pol_min <- function(longitude, latitude, ...) {

  sp_polygon <- find_prefs(longitude = longitude, latitude = latitude) %>%
    use_series(pref_code) %>%
    purrr::map(
     jpn_pref
    ) %>%
    purrr::reduce(rbind)

  which_row <- suppressMessages(grep(TRUE, sf::st_intersects(sp_polygon,
                                                           sf::st_point(c(longitude, latitude), dim = "XY"),
                                                           sparse = FALSE)))

  res <- list(spdf = sp_polygon, which = which_row)
  return(res)
}


crs_4326 <- structure(
  list(epsg = 4326L, proj4string = "+proj=longlat +datum=WGS84 +no_defs"),
  class = "crs")

tweak_sf_output <- function(target) {

  target <- target %>% sf::st_sf()

  if (identical(sf::st_crs(target)$proj4string, crs_4326) != TRUE) {
    target <- target %>% sf::st_transform(crs = 4326)
  }

  res <- target %>%
    tibble::as_tibble() %>% sf::st_sf()

  return(res)
}
