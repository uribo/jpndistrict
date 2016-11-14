#' Read administration office point datasets.
#'
#' @description Name and geolocations for administration offices in prefecture.
#' @param code prefecture code
#' @param path shapefile path
#' @importFrom readr read_rds
#' @examples
#' \dontrun{
#' read_ksj_p33(code = 17)
#' }
#' @export
read_ksj_p33 <- function(code = NULL, path = NULL) {

  download.file <- unzip <- NULL

  df.dl.url <- readr::read_rds(system.file("extdata/ksj_P33_index.rds", package = "jpndistrict"))

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

  collect_ksj_p33(code = code, path = path)
}
