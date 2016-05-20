#' Read data from file path in package
#'
#' @export
#' @param x File name, e.g., github_commits_2016-05-19.csv. Required.
#' @details file must be in \code{extdata}
#' @return a data.frame
get_file <- function(x) {
  ff <- system.file("extdata", x, package = "rostats")
  if (ff == "") stop("file ", x, " does not exist", call. = FALSE)
  dat <- read.csv(ff, stringsAsFactors = FALSE)
  dat$date <- as.Date(dat$date)
  return(dat)
}
