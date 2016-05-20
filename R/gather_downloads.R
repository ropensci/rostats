#' Get rOpenSci commits
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
#' @return writes a data.frame to extdata/, returns path to file
#' @examples \dontrun{
#' pkgs <- ropensci_pkgs(TRUE)$name
#' gather_downloads(x = pkgs[1:10])
#' # gather_downloads(x = pkgs)
#' }
gather_downloads <- function(x, from = NULL, to = Sys.Date() - 1, file = Sys.Date()) {
  out <- lapply(x, function(z) {
    tmp <- cran_first_date(gather_cran(z))
    dd <- cranlogs::cran_downloads(z, from = tmp$date, to = to)
    row.names(dd) <- NULL
    dd
  })
  df <- dplyr::bind_rows(out)
  ff <- paste0("extdata/cran_downloads_", file, ".csv")
  write.csv(df, file = ff, row.names = FALSE)
  message("data written to ", ff)
}