#' Cummulative commits plot
#'
#' @export
#' @param x File name, e.g., github_commits_2016-05-19.csv. Required.
#' @param exclude (character) github user names to exclude
#' @param exclude_core (logical) exclude core user names. Default: \code{FALSE}
#' @examples \dontrun{
#' cum_contribs("github_commits_2016-05-19.csv")
#' cum_contribs("github_commits_2016-05-19.csv", exclude_core = TRUE)
#' }
cum_contribs <- function(x, exclude = c("sckott", "karthik", "cboettig", "jeroenooms"),
                         exclude_core = FALSE) {
  dat <- dplyr::tbl_df(get_file(x))
  if (exclude_core) dat <- dplyr::filter(dat, !author %in% exclude)

  ## summarise
  firstdates <- dat %>%
    select(-pkg) %>%
    group_by(author) %>%
    arrange(date) %>%
    filter(rank(date, ties.method = "first") == 1) %>%
    ungroup() %>%
    mutate(count = 1) %>%
    arrange(date) %>%
    mutate(cumsum = cumsum(count))

  ## plot
  ggplot(firstdates, aes(date, cumsum)) +
    geom_line(size = 2) +
    theme_grey(base_size = 18) +
    scale_x_date(labels = scales::date_format("%Y/%m")) +
    labs(x = 'May 2011 to May 2016', y = 'Cumulative New Contributors')
}
