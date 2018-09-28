globalVariables("jpnprefs")
globalVariables("prefecture_mesh")

#' Prefectural informations in Japan
#'
#' @description Prefectures dataset.
#' @format A data frame with 47 rows 11 variables:
#' \itemize{
#'   \item{jis_code: jis code}
#'   \item{prefecture: prefecture names}
#'   \item{capital: capital name for prefecture}
#'   \item{region: region}
#'   \item{major_island: }
#'   \item{prefecture_en: }
#'   \item{capital_en: }
#'   \item{region_en: }
#'   \item{major_island_en: }
#'   \item{capital_latitude: latitude for catital}
#'   \item{capital_longitude: longitude for catital}
#' }
"jpnprefs"

#' Prefecture's meshcode
#'
#' @description Prefectures dataset.
#' @format A simple feature data frame with 314 rows 5 variables:
#' \itemize{
#'   \item{prefcode: prefecture code}
#'   \item{meshcode}
#'   \item{name}
#'   \item{type}
#'   \item{geometry}
#' }
"prefecture_mesh"
