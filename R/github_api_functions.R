get_token <- function(x = NULL) {
  if (is.null(x)) {
    x <- Sys.getenv('GITHUB_PAT')
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
  #token <- github_auth()
  token <- get_token(token)
  outout <- list(); iter <- 0; nexturl <- "dontstop"
  while (nexturl != "stop") {
    iter <- iter + 1
    req <- if (grepl("https:/", nexturl)) {
      httr::GET(nexturl, token)
    } else {
      httr::GET(make_url(repo, "commits"),
          query = list(per_page = 100, since = since), token)
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

ct <- function(l) Filter(Negate(is.null), l)
