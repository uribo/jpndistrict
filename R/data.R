globalVariables("jpnprefs")
globalVariables("prefecture_mesh")

#' Prefectural informations in Japan
#'
#' @description Prefectures dataset.
#' @format A data frame with 47 rows 7 variables:
#' \itemize{
#'   \item{jis_code: jis code}
#'   \item{prefecture: prefecture names}
#'   \item{capital: capital name for prefecture}
#'   \item{region: region}
#'   \item{major_island: }
#'   \item{capital_latitude: latitude for catital}
#'   \item{capital_longitude: longitude for catital}
#' }
"jpnprefs"

#' Prefecture's meshcode
#'
#' @description Prefectures dataset.
#' @format A simple feature data frame with 314 rows 6 variables:
#' \itemize{
#'   \item{pref_code: prefecture code}
#'   \item{prefecture: name}
#'   \item{city_code: city code (JIS code)}
#'   \item{city: name}
#'   \item{geometry}
#' }
"prefecture_mesh"
