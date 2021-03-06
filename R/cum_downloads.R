#' Cummulative downloads plot
#'
#' @export
#' @param x File name, e.g., cran_downloads_2016-05-19.csv. Default is \code{NULL},
#' and if so, we look for file with most recent date in its file name in
#' \code{rappdirs::user_cache_dir("rostats")} + "/cran_downloads/"
#' @param top_n (numeric/integer) number of packages to plot data for,
#' starting from the most downloaded
#' @examples \dontrun{
#' cum_downloads()
#' }
cum_downloads <- function(x = NULL, top_n = 10) {

  if (!class(top_n) %in% c("numeric", "integer")) {
    stop("top_n must be numeric or integer", call. = FALSE)
  }
  dat <- dplyr::tbl_df(get_cran(x))

  ## summarise
  dat <- dplyr::bind_rows(lapply(split(dat, dat$package), function(z) {
    pkg <- z$package[1]
    z <- dplyr::group_by(z, date)
    z <- dplyr::summarise(z, count = sum(count))
    z <- dplyr::mutate(z, cumsum = cumsum(count))
    z$package <- pkg
    z
  }))

  sums <- sort(vapply(split(dat, dat$package), function(z) z[NROW(z), "cumsum"][[1]], 1), decreasing = TRUE)
  dat <- dplyr::filter(dat, package %in% names(sums[1:top_n]))

  ## plot
  ggplot(dat, aes(date, cumsum)) +
    geom_line(size = 2) +
    theme_grey(base_size = 18) +
    facet_wrap(~package, scales = "free_y") +
    labs(x = NULL, y = 'Cumulative CRAN Downloads')
}
