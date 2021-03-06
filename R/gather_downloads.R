#' Get CRAN downloads
#'
#' @export
#' @param x One or more R packages, of the form \code{ropensci/rfishbase},
#' case sensitive
#' @param from (Date) A date to start from. Default is \code{NULL}, and we
#' get the first date published on CRAN to start from using \code{\link{gather_cran}}
#' and \code{\link{cran_first_date}}
#' @param to (Date) A date to end at. Default: \code{Sys.Date() - 1}
#' @param file A date, or other character string to make the file
#' ending unique. Default: \code{Sys.Date()}
#' @return writes a data.frame to \code{rappdirs::user_cache_dir("rostats")} +
#' '/cran_downloads/', prints message of path to file, and returns file path itself
#' @examples \dontrun{
#' library("ropkgs")
#' library("dplyr")
#' df <- ro_pkgs()
#' pkgs <- tbl_df(df$packages) %>%
#'   filter(on_cran) %>%
#'   .$name %>%
#'   sort
#' pkgs <- sub("redland-bindings", "redland", pkgs)
#' gather_downloads(x = pkgs[1])
#' # gather_downloads(x = pkgs[1:3])
#' # gather_downloads(x = pkgs)
#' }
gather_downloads <- function(x, from = NULL, to = Sys.Date() - 1, file = Sys.Date()) {
  out <- plyr::llply(x, function(z) {
    tmp <- cran_first_date(gather_cran(z))
    if (NROW(stats::na.omit(tmp)) == 0) return(dplyr::data_frame())
    if (NROW(tmp) > 1) tmp <- tmp[NROW(tmp),]
    if (tmp$date == Sys.Date()) return(dplyr::data_frame())
    if (tmp$date > to) return(dplyr::data_frame())
    dd <- cranlogs::cran_downloads(z, from = tmp$date, to = to)
    row.names(dd) <- NULL
    dd
  }, .inform = TRUE)
  df <- dplyr::bind_rows(out)
  dir <- file.path(rappdirs::user_cache_dir("rostats"), "cran_downloads")
  if (!file.exists(dir)) dir.create(dir, recursive = TRUE)
  ff <- file.path(dir, paste0("cran_downloads_", file, ".csv"))
  utils::write.csv(df, file = ff, row.names = FALSE)
  message("data written to ", ff)
  return(ff)
}
