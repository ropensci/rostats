#' Cummulative commits plot
#'
#' @export
#' @param x File name, e.g., github_commits_2016-05-19.csv. Required.
#' @param exclude (character) github user names to exclude
#' @param exclude_core (logical) exclude core user names. Default: \code{FALSE}
#' @examples \dontrun{
#' cum_commits(x = "github_commits_2016-05-23.csv")
#' cum_commits(x = "github_commits_2016-05-23.csv", exclude_core = TRUE)
#' }
cum_commits <- function(x, exclude = c("sckott", "karthik", "cboettig", "jeroenooms"),
                        exclude_core = FALSE) {
  dat <- dplyr::tbl_df(get_file(x))
  if (exclude_core) dat <- dplyr::filter(dat, !author %in% exclude)

  ## summarise
  dat <- dat %>%
    group_by(date) %>%
    summarise(count = n()) %>%
    mutate(cumsum = cumsum(count))

  ## plot
  ggplot(dat, aes(date, cumsum)) +
    geom_line(size = 2) +
    theme_grey(base_size = 18) +
    scale_x_date(labels = scales::date_format("%Y/%m")) +
    labs(x = 'May 2011 to May 2016', y = 'Cumulative Code Contributions')
}
