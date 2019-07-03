#' get date that submissions were approved in ropensci software review repo
#'
#' @export
#' @param token (optional) a github PAT
#' @examples
#' library(dplyr)
#' # all approved packages
#' df <- ropensci_review_dates()
#' # filter to some subset
#' ## those approved in 2018
#' tbl_df(df) %>%
#'   arrange(date_approved) %>%
#'   filter(date_approved >= "2018-01-01", date_approved < "2019-01-01") %>%
#'   data.frame %>%
#'   NROW
ropensci_review_dates <- function(token = NULL) {
  tmp <- gh_repo_issues_pagination("ropensci", "software-review", "open", per_page = 100)
  closed1 <- gh_repo_issues_pagination("ropensci", "software-review", "closed", per_page = 100)
  closed2 <- gh_repo_issues_pagination("ropensci", "software-review", "closed", per_page = 100, page = 2)
  alldata <- dplyr::bind_rows(tmp, closed1, closed2)

  res <- c()
  for (i in seq_len(NROW(alldata))) {
    if (any(grepl("approved", alldata[i, "labels"][[1]]$name))) {
      res[i] <- alldata$number[i]
    }
  }
  res <- stats::na.omit(res)
  # length(res)

  output <- list()
  output2 <- list()
  res2 <- res[49:length(res)]
  for(i in seq_along(res2)) {
    cat(i, '\n')
    if (!res2[i] %in% c(156, 139)) {
      out <- gh_events_count("ropensci/software-review", res2[i])
      ff <- out %>% filter(event == "labeled", label.name == '6/approved')
      output2[[i]] <- data.frame(issue = res2[i], date_approved = ff$created_at,
                                stringsAsFactors = FALSE)
    }
  }
  df <- dplyr::bind_rows(c(output, output2))
  readr::write_csv(df, "ropensci_review_dates_2019-01-10.csv")
  return(df)
}

gh_events_count <- function(repo, review_issue, token = NULL, ...) {
  url <- make_url(repo, file.path("issues", review_issue, "events"))
  gh_while(url, ct(list(per_page = 100, since = NULL)), token, ...)
}
