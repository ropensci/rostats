`%||%` <- function(x, y) if (is.null(x)) y else x

unconf_issues <- function(repo, token = NULL, ...) {
  token <- get_token(token)
  res <- httr::GET(make_url("ropensci/unconf17", "issues"),
                   query = list(per_page = 100), token, httr::verbose())
  process_result(res)
}

library("ropkgs")
library("dplyr")
dat <- unconf_issues()

# those that are lang R and are using sudo=required && scott's
tbl_df(dat) %>%
  select(title, user.login) %>%
  readr::write_csv(., path = "unconf_issues.csv")
