## Functions
github_auth <- function(appname = getOption("gh_appname"), key = getOption("gh_id"),
                        secret = getOption("gh_secret")) {
  if (is.null(getOption("gh_token"))) {
    myapp <- oauth_app(appname, key, secret)
    token <- oauth2.0_token(oauth_endpoints("github"), myapp)
    options(gh_token = token)
  } else {
    token <- getOption("gh_token")
  }
  return(token)
}

make_url <- function(x, y, z) {
  sprintf("https://api.github.com/repos/%s/%s/%s", x, y, z)
}

process_result <- function(x) {
  stop_for_status(x)
  if (!x$headers$`content-type` == "application/json; charset=utf-8")
    stop("content type mismatch")
  tmp <- content(x, as = "text")
  jsonlite::fromJSON(tmp, flatten = TRUE)
}

gh_rate_limit <- function(...) {
  token <- github_auth()
  req <- GET("https://api.github.com/rate_limit", config = c(token = token, ...))
  process_result(req)
}

# 2011-05-04 was our first commit to ropensci
# a few others are earlier, but not really ropensci joints: rdataone, rrdf
gh_commits_count <- function(repo, owner = "ropensci", since = "2011-05-03T00:00:00Z", ...) {
  token <- github_auth()
  outout <- list(); iter <- 0; nexturl <- "dontstop"
  while (nexturl != "stop") {
    iter <- iter + 1
    req <- if (grepl("https:/", nexturl)) {
      GET(nexturl, config = c(token = token))
    } else {
      GET(make_url(owner, repo, "commits"),
          query = list(per_page = 100, since = since),
          config = c(token = token))
    }
    outout[[iter]] <- process_result(req)
    link <- req$headers$link
    nexturl <- if (is.null(link)) { "stop" } else {
      if (grepl("next", link)) {
        stringr::str_extract(link, "https://[0-9A-Za-z/?=\\._&\\%-]+")
      } else {
        "stop"
      }
    }
  }
  outout <- outout[sapply(outout, function(x) !identical(x, list()))]
  dplyr::rbind_all(outout)
}

ct <- function(l) Filter(Negate(is.null), l)
