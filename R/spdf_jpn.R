#' Spatial Data frame for prefecture area polygon
#'
#' @description Prefecture polygon data.
#' @details Download administrative area data from the National Land Numeral Information Download Service. If there is already a downloaded file,
#' it is read by giving the path to the file as the path argument. When path is not specified, it downloads to the temporary folder.
#' In the case of the same prefecture in the session, we will not download twice.
#' @param path path to ksj N03 folder
#' @param code jis code from 1 to 47
#' @param admin_name string
#' @param district logical (default TRUE)
#' @examples
#' \dontrun{
#' spdf_jpn_pref(code = 33, district = FALSE)
#' }
#'
#' @export
spdf_jpn_pref <- function(path, code = NULL, admin_name = NULL, district = TRUE) {

  if (missing(path)) {
    if (missing(admin_name)) {
      pref.code <- collect_prefcode(code = code)
    } else if (missing(code)) {
      pref.code <- collect_prefcode(admin_name = admin_name)
    }

    d <- read_ksj_cityarea(code = as.numeric(pref.code))

  } else {
    d <- read_ksj_cityarea(path = path)
  }

  if (district == TRUE) {
    res <- d
  } else {
    res <- raw_bind_cityareas(d)
  }
  return(res)
}

#' Spatial Data frame for city area polygons
#'
#' @description City area polygon data. When an administrative name (jis_code_city) or code (jis_code_city)
#' is specified as an argument, the target city data is extracted. If neither is given,
#' it becomes the data of the target prefecture.
#' @import spdplyr
#' @importFrom dplyr filter
#' @param path path to ksj N03 folder
#' @param jis_code_pref jis code from 1 to 47
#' @param jis_code_city jis code for city as jis_code_pref + identifier number
#' @param admin_name city name
#' @examples
#' spdf_jpn_cities(jis_code_pref = 33, jis_code_city = 33103)
#' spdf_jpn_cities(jis_code_pref = 33, jis_code_city = c(33103, 33104, 33205))
#' @export
spdf_jpn_cities <- function(path, jis_code_pref, jis_code_city = NULL, admin_name = NULL) {

  city_code <- city_name_full <- NULL

  if (missing(path)) {
      d <- spdf_jpn_pref(code = jis_code_pref, district = TRUE)
    } else {
      d <- spdf_jpn_pref(path = path, district = TRUE)
    }

  if (missing(admin_name) & missing(jis_code_city)) {
    d <- d
  } else if (missing(admin_name)) {
    d <- dplyr::filter(d, city_code %in% jis_code_city)
  } else if (missing(jis_code_city)) {
    d <- dplyr::filter(d, grepl(admin_name, city_name_full))
  }

  return(d)
}

#' Spatial Dataframe for administration office points
#'
#' @description Name and geolocations for administration offices in prefecture.
#' @param code prefecture code
#' @param path shapefile path
#' @param jis_code_city jis code for city as jis_code_pref + identifier number
#' @importFrom dplyr filter
#' @return data.frame. contains follow columns jis_code, type, name, address, longitude and latitude.
#' @examples
#' \dontrun{
#' spdf_jpn_admins(code = 17)
#' }
#' @export
spdf_jpn_admins <- function(path, code = NULL, jis_code_city = NULL) {

  jis_code <- NULL

  if (missing(path)) {
      pref.code <- collect_prefcode(code = code)
      d <- read_ksj_p34(code = as.numeric(pref.code))
  } else {
    d <- read_ksj_p34(path = path)
  }

  if (is.null(jis_code_city)) {
    res <- d
  } else {
    res <- filter(d, jis_code %in% jis_code_city)
  }
  return(res)

}
