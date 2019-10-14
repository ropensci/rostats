get_token <- function(x = NULL) {
  if (is.null(x)) {
    x <- Sys.getenv('GITHUB_PAT_ROSTATS')
    if (nchar(x) == 0) {
      stop("no Github PAT found", call. = FALSE)
    }
  }
  httr::add_headers(Authorization = paste0("token ", x))
}

make_url <- function(x, y) {
  sprintf("https://api.github.com/repos/%s/%s", x, y)
}

process_result <- function(x) {
  httr::stop_for_status(x)
  if (!x$headers$`content-type` == "application/json; charset=utf-8") {
    stop("content type mismatch", call. = FALSE)
  }
  tmp <- httr::content(x, as = "text", encoding = "UTF-8")
  jsonlite::fromJSON(tmp, flatten = TRUE)
}

gh_rate_limit <- function(token = NULL, ...) {
  req <- httr::GET("https://api.github.com/rate_limit", get_token(token), ...)
  process_result(req)
}

# 2011-05-04 was our first commit to ropensci
gh_commits_count <- function(repo, token = NULL, since = "2011-05-03T00:00:00Z", ...) {
  url <- make_url(repo, "commits")
  gh_while(url, ct(list(per_page = 100, since = since)), token, ...)
}

ct <- function(l) Filter(Negate(is.null), l)


stats_contribs <- function(repo, token = NULL, ...) {
  token <- get_token(token)
  res <- httr::GET(make_url(repo, "stats/contributors"), token, ...)
  process_result(res)
}

gh_user <- function(x, token = NULL, ...) {
  res <- httr::GET(paste0("https://api.github.com/users/", x), get_token(token), ...)
  process_result(res)
}

gh_org_issues <- function(x, token = NULL, ...) {
  res <- httr::GET(sprintf("https://api.github.com/orgs/%s/issues", x), get_token(token), ...)
  process_result(res)
}

gh_repo_issues <- function(owner, repo, state = "open", since = NULL, token = NULL, ...) {
  res <- httr::GET(
    sprintf("https://api.github.com/repos/%s/%s/issues", owner, repo),
    query = ct(list(state = state, since = since)),
    get_token(token),
    ...
  )
  process_result(res)
}

# get stats for all releases for a repo
# gh_releases_stats(owner = "ropensci", repo = "rfishbase", release_id = 17860885)
gh_releases_stats <- function(owner, repo, release_id, ...) {
  res <- httr::GET(
    sprintf("https://api.github.com/repos/%s/%s/releases/%s", owner, repo, release_id),
    get_token(token)
    #...
  )
  process_result(res)
}

gh_repo_issues_pagination <- function(owner, repo, state = "open", since = NULL, token = NULL,
                                      per_page = NULL, page = NULL, ...) {
  res <- httr::GET(
    sprintf("https://api.github.com/repos/%s/%s/issues", owner, repo),
    query = ct(list(state = state, since = since, per_page = per_page, page = page)),
    get_token(token),
    ...
  )
  process_result(res)
}

gh_issue_comments <- function(owner, repo, since = NULL, token = NULL, ...) {
  url <- sprintf("https://api.github.com/repos/%s/%s/issues/comments", owner, repo)
  gh_while(url, ct(list(since = since)), token, ...)
}

gh_repos <- function(owner, type = "public", token = NULL, ...) {
  url <- sprintf("https://api.github.com/orgs/%s/repos", owner)
  gh_while(url, ct(list(per_page = 100, since = NULL)), token, ...)
}


gh_while <- function(url, args, token, ...) {
  token <- get_token(token)
  outout <- list(); iter <- 0; nexturl <- "dontstop"
  while (nexturl != "stop") {
    iter <- iter + 1
    req <- if (grepl("https:/", nexturl)) {
      httr::GET(nexturl, token, ...)
    } else {
      httr::GET(url, query = args, token, ...)
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
  dplyr::bind_rows(outout)
}
