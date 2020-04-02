#' Collect administration office point datasets.
#'
#' @param path path to P34 shapefile (if already exist)
#' @importFrom sf st_read
#' @importFrom dplyr mutate if_else
#' @importFrom purrr set_names
collect_ksj_p34 <- function(path = NULL) {
  jis_code <- NULL
  code <- gsub(".+P34-14_|_GML|/", "", path)
  d <- sf::st_read(
    paste0(path, "/", list.files(path, pattern = paste0(code, ".shp$"))),
    stringsAsFactors = FALSE,
    as_tibble = TRUE,
    crs = 4612,
    options = "ENCODING=cp932") %>%
    purrr::set_names(
      c("jis_code", "type", "name", "address", "geometry")) %>%
    dplyr::mutate(jis_code = as.factor(jis_code))
  return(d)
}

#' Intermediate function
#'
#' @param pref sf object (prefecture)
#' @importFrom dplyr mutate
#' @importFrom sf st_buffer st_sf st_union
raw_bind_cityareas <- function(pref) {
  tmp_union <-
    suppressMessages(suppressWarnings(sf::st_buffer(pref, 0) %>%
                                        sf::st_union() %>%
                                        sf::st_sf()))
  df_res <- suppressMessages(
    suppressWarnings(
      tmp_union %>%
        dplyr::mutate(
          pref_code  = as.numeric(substr(pref$city_code[1], 1, 2)),
          prefecture = pref$prefecture[1]
        ) %>%
        sf::st_sf() %>%
        sf::st_buffer(dist = 0.001)
    ) %>%
      dplyr::select(
        pref_code = 1,
        prefecture = 2,
        geometry = 3
      ))
  return(df_res)
}

#' Intermediate function
#'
#' @description Download N03 raw data files or loading if file exists.
#' @param code prefecture code (JIS X 0402)
#' @param path path to N03 shapefile (if already exist)
read_ksj_cityarea <- function(code = NULL, path = NULL) {
  # nocov start
  if (missing(path)) {
    path <- path_ksj_cityarea(code)
  }
  res <- collect_cityarea(path)
  return(res)
  # nocov end
}

#' Download KSJ N03 zip files
#'
#' @param code prefecture code (JIS X 0402)
#' @param path path to N03 shapefile (if already exist)
#' @importFrom utils download.file
#' @importFrom utils unzip
path_ksj_cityarea <- function(code = NULL, path = NULL) {
  # nocov start
  if (missing(path)) {
    pref_identifer <- sprintf("%02d", code)
    dest_path <-
      paste(tempdir(),
            paste0("N03-150101_", pref_identifer, "_GML.zip"),
            sep = "/")
    extract_path <- paste(tempdir(), pref_identifer,  sep = "/")
    # ksj zip file none
    if (is.null(path) & file.exists(dest_path) == FALSE) {
      utils::download.file(
        paste0(
          "http://nlftp.mlit.go.jp/ksj/gml/data/N03/N03-2015/N03-150101_",
          pref_identifer,
          "_GML.zip"
        ),
        destfile <- dest_path,
        method   <- "auto",
        quiet    <- TRUE
      )
      utils::unzip(zipfile = dest_path,
                   exdir   = extract_path)
      path <- paste(extract_path, gsub(
        ".zip$",
        "",
        paste0("N03-20150101_", pref_identifer, "_GML.zip")
      ), sep = "/")

      # ksj zip file exist
    } else if (file.exists(dest_path) == TRUE) {
      path <- extract_path
    }
  }
  return(path)
  # nocov end
}

#' Get prefecture code (JIS X 0402)
#'
#' @description Get prefecture code from prefecture of name or number.
#' @param code numeric
#' @param admin_name prefecture code for Japanese (character)
#' @importFrom dplyr filter mutate pull
#' @importFrom purrr pmap_chr
collect_prefcode <- function(code = NULL, admin_name = NULL) {
  jis_code <- prefecture <- NULL
  if (missing(admin_name)) {
    pref_code <-
      dplyr::filter(jpnprefs, jis_code == code_validate(code)$code) %>%
      dplyr::pull(jis_code)
  } else if (missing(code)) {
    pref_code <-
      dplyr::filter(jpnprefs, prefecture == admin_name) %>%
      dplyr::pull(jis_code)
  }
  return(pref_code)
}


#' Collect administration area
#'
#' @param path path to N03 shapefile (if already exist)
#' @import dplyr
#' @import sf
collect_cityarea <- function(path = NULL) {
  # nocov start
  . <- N03_001 <- N03_002 <- N03_003 <- N03_004 <- N03_007 <- tmp_var <- NULL # nolint
  pref_name <-
    city_name_ <- city_name <- city_name_full <- city_code <- geometry <- NULL # nolint
  res <-
    suppressWarnings(
      sf::st_read(
        list.files(
          path,
          pattern = "shp$",
          full.names = TRUE,
          recursive = TRUE),
        crs = 4612,
        as_tibble = TRUE,
        stringsAsFactors = FALSE) %>%
        sf::st_simplify(preserveTopology = FALSE, dTolerance = 0.001) %>%
        dplyr::filter(sf::st_is_empty(.) == FALSE) %>%
        dplyr::mutate(
          tmp_var = dplyr::if_else(is.na(N03_003), "", N03_003),
          city_name_full = gsub("[[:space:]]", "", gsub("NA", "", paste(tmp_var, N03_004))) # nolint
        ) %>%
        dplyr::rename(
          pref_name  = N03_001,
          city_name_ = N03_003,
          city_name  = N03_004,
          city_code  = N03_007
        ) %>%
        dplyr::mutate_at(.vars = dplyr::vars(dplyr::contains("name")),
                  iconv,
                  to = "UTF8") %>%
        dplyr::select(pref_name,
                      city_name_, city_name, city_name_full, city_code,
                      geometry)
    )
  return(res)
  # nocov end
}


#' Intermediate function
#'
#' @param pref_code prefecture code (JIS X 0402)
#' @param path path to P34 shapefile (if already exist)
#' @importFrom utils download.file
#' @importFrom utils unzip
read_ksj_p34 <- function(pref_code = NULL, path = NULL) {
  # nolint start
  if (missing(path)) {
    df_df_url <-
      readRDS(system.file("extdata/ksj_P34_index.rds",
                          package = "jpndistrict"))
    if (is.null(path) &
        file.exists(paste(tempdir(),
                          df_df_url$dest_file[pref_code], sep = "/")) == FALSE) {
      utils::download.file(
        df_df_url$zipFileUrl[pref_code],
        destfile = paste(tempdir(), df_df_url$dest_file[pref_code], sep = "/"),
        method = "auto"
      )
      utils::unzip(
        zipfile = paste(tempdir(), df_df_url$dest_file[pref_code], sep = "/"),
        exdir   = paste(tempdir(), gsub(".zip", "", df_df_url$dest_file[pref_code]), sep = "/")
      )
      path <- paste(tempdir(), gsub(".zip", "", df_df_url$dest_file[pref_code]), sep = "/")
    } else if (file.exists(paste(tempdir(), df_df_url$dest_file[pref_code], sep = "/")) == TRUE) {
      path <- paste(tempdir(), gsub(".zip", "", df_df_url$dest_file[pref_code]), sep = "/") # nocov
    }
    res <- collect_ksj_p34(path = path)
  } else {
    res <- collect_ksj_p34(path = path) # nocov
  }
  return(res)
  # nolint end
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
  pref_code <- NULL
  pref_code_chr <-
    find_prefs(longitude = longitude, latitude = latitude) %>%
    dplyr::pull(pref_code)
  sp_polygon <- NULL
  which_row  <- integer(0)
  if (identical(pref_code_chr, character(0)) == TRUE) {
    1
  } else {
    sp_polygon <-
      pref_code_chr %>%
      purrr::map(jpn_pref) %>%
      purrr::reduce(rbind)
    x <-
      sf::st_point(c(longitude, latitude), dim = "XY")
    which_row <-
      suppressMessages(grep(
        TRUE,
        sf::st_intersects(sp_polygon,
                          x,
                          sparse = FALSE)
      ))
    if (length(which_row) > 1) {
      if (!requireNamespace("lwgeom", quietly = TRUE))
        rlang::abort("lwgeom required: install that first") # nocov
      which_row <-
        which.min(sf::st_distance(st_sfc(x, crs = 4326),
                                  sp_polygon,
                                  by_element = TRUE))
      sp_polygon <-
        jpn_pref(pref_code = which_row)
    }
  }
  list(spdf = sp_polygon, which = which_row)
}

crs_4326 <-
  structure(list(epsg = 4326L,
                 proj4string = "+proj=longlat +datum=WGS84 +no_defs"),
            class = "crs")

tweak_sf_output <- function(target) {
  target <-
    sf::st_sf(target)
  if (utils::packageVersion("sf") <= numeric_version("0.8.1")) {
    if (identical(sf::st_crs(target)$proj4string, crs_4326) != TRUE) {
      target <- sf::st_transform(target, crs = 4326)
    }
  } else {
    if (identical(sf::st_crs(target)$input, "EPSG:4326") != TRUE) {
      target <- sf::st_transform(target, crs = 4326)
    }
  }
  res <-
    target %>%
    tibble::as_tibble() %>%
    sf::st_sf()
  return(res)
}

sfg_point_as_coords <- function(geometry) {
  if (sf::st_is(geometry, "POINT")) {
      list(longitude = sf::st_coordinates(geometry)[1],
           latitude =  sf::st_coordinates(geometry)[2])
    }
}

collapse_int2utf8 <- function(var) {
  paste(intToUtf8(var, multiple = TRUE), collapse = "")
}

export_pref_80km_mesh <- function(code, ...) {
  meshcode <- NULL
  sf_pref <-
    jpn_pref(pref_code = code) %>%
    lwgeom::st_make_valid()
  res <- suppressMessages(jpmesh::sf_jpmesh %>%
                            sf::st_join(sf_pref,
                                        sf::st_overlaps,
                                        left = FALSE) %>%
                            dplyr::pull(meshcode) %>%
                            unique())
  return(res)
}

mesh_intersect <- function(data, x) {
  res_contains <- NULL
  df_tmp <- tibble::tibble(
    res_contains = suppressMessages(
      rowSums(sf::st_intersects(data,
                                x,
                                sparse = FALSE))))
  df_tmp$id <- seq_len(nrow(df_tmp))
  data[df_tmp %>%
         dplyr::filter(res_contains != 0) %>%
         dplyr::pull(id) %>%
         unique(), ]
}

mesh_intersect_filter <- function(data) {
  . <- meshcode <- out <- NULL # nolint
  data %>%
    dplyr::pull(meshcode) %>%
    purrr::map(jpmesh::fine_separate) %>%
    rlang::flatten_chr() %>%
    unique() %>%
    tibble::enframe(name = NULL, value = "meshcode") %>%
    dplyr::mutate(out = purrr::map(meshcode, ~ jpmesh::mesh_to_coords(.x))) %>%
    tidyr::unnest_wider(col = out) %>%
    dplyr::select(meshcode, tidyselect::everything()) %>%
    dplyr::mutate(geometry = purrr::pmap(., ~ jpmesh:::mesh_to_poly(...))) %>%
    sf::st_sf(crs = 4326, stringsAsFactors = FALSE)
}

decode.sfencoded <- function(x, crs = 4326) {
  geometry <- NULL
  googlePolylines::polyline_wkt(x) %>%
    dplyr::mutate(geometry = sf::st_as_sfc(geometry)) %>%
    sf::st_sf(crs = crs)
}

decode.sf <- function(x) {
  crs <- st_crs(x)
  geometry <- NULL
  googlePolylines::encode(x) %>%
    googlePolylines::polyline_wkt() %>%
    dplyr::mutate(geometry = sf::st_as_sfc(geometry)) %>%
    sf::st_sf(crs = crs)
}
