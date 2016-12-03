#' Display city map based on 'ggplot2'
#'
#' @description Rendering city map using 'ggplot2' and 'ggghost' systems.
#' @import ggplot2
#' @import ggghost
#' @importFrom ggthemes theme_map
#' @param df data frame
#' @param ghost logical (default TRUE). If don't spirit of ggplot2 calls, specify FALSE.
#' @param ... Other arguments passed on to coord_map
#' @aliases gg_jpn_district
#' @examples
#' \dontrun{
#' gg_jpn_district(spdf_jpn_pref(code = 33, district = TRUE))
#' }
#' @export
gg_jpn_district <- function(df, ghost = TRUE, ...) {
  p <- long <- lat <- NULL

  df <- ggplot2::fortify(df)

  if (ghost == TRUE) {
    p %g<% ggplot2::ggplot(data = df, ggplot2::aes(map_id = id, x = long, y = lat))
  } else {
    p <- ggplot2::ggplot(data = df, ggplot2::aes(map_id = id, x = long, y = lat))
  }

  p <- p + ggplot2::geom_map(map = df, color = "black", fill = "white")
  p <- p + ggplot2::coord_equal()
  p <- p + ggplot2::coord_map(...)
  p <- p + ggthemes::theme_map()
  return(p)
}
