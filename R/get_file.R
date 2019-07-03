#' Read data from file path in package
#'
#' @export
#' @param x File name, e.g., github_commits_2016-05-19.csv. Required.
#' @details file must be in \code{extdata}
#' @return a data.frame
get_cran <- function(x) {
  dir <- file.path(rappdirs::user_cache_dir("rostats"), "cran_downloads")
  get_file(dir, x, "gather_downloads()")
}

#' @export
#' @rdname get_cran
get_github <- function(x) {
  dir <- file.path(rappdirs::user_cache_dir("R/rostats"), "commits")
  get_file(dir, x, "gather_commits()")
}

get_file <- function(dir, x, y) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  if (is.null(x)) {
    if (length(list.files(dir, pattern = ".csv")) == 0) stop("No files downloaded yet :(", call. = FALSE)
    files <- list.files(dir)
    files <- as.Date(gsub("cran_downloads_|github_commits_|\\.csv", "", files))
    names(files) <- list.files(dir)
    files <- sort(files)
    x <- names(files[length(files)])
  }
  ff <- file.path(dir, x)
  if (!file.exists(ff)) stop("file ", x, sprintf(" does not exist\n  Run %s to get data", y), call. = FALSE)
  dat <- utils::read.csv(ff, stringsAsFactors = FALSE)
  dat$date <- as.Date(dat$date)
  return(dat)
}
