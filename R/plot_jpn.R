#' Display city map based on 'ggplot2'
#'
#' @description Rendering city map using 'ggplot2' and 'ggghost' system.
#' @import ggplot2
#' @import ggghost
#' @importFrom ggthemes theme_map
#' @param df data frame
#' @aliases gg_jpn_district
#' @examples
#' \dontrun{
#' gg_jpn_district(spdf_jpn_pref(code = "37", district = TRUE))
#' }
#' @export
gg_jpn_district <- function(df) {
  p <- long <- lat <- NULL

  df <- ggplot2::fortify(df)


  p %g<% ggplot2::ggplot(data = df, ggplot2::aes(map_id = id, x = long, y = lat))
  p <- p + ggplot2::geom_map(map = df, color = "black", fill = "white")
  p <- p + ggplot2::coord_map()
  p <- p + ggthemes::theme_map()
  return(p)
}
