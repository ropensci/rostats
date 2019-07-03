# "GET /repos/:owner/:repo/contents/:path"
library("crul")
library("ropkgs")
library("dplyr")

get_token <- function(x = NULL) {
  if (is.null(x)) {
    x <- Sys.getenv('GITHUB_PAT_ROSTATS')
    if (nchar(x) == 0) {
      stop("no Github PAT found", call. = FALSE)
    }
  }
  list(Authorization = paste0("token ", x))
}

process_result <- function(x) {
  x$raise_for_status()
  if (!x$response_headers$`content-type` == "application/json; charset=utf-8") {
    stop("content type mismatch", call. = FALSE)
  }
  tmp <- x$parse("UTF-8")
  jsonlite::fromJSON(tmp, flatten = TRUE)
}

get_x_file <- function(repo, path, token = NULL, ...) {
  token <- get_token(token)
  cli <- crul::HttpClient$new(
    url = make_url(repo, path),
    headers = token,
    opts = list(...)
  )
  res <- cli$get()
  exists <- TRUE
  out <- NULL
  if (res$status_code != 200) {
    exists <- FALSE
  } else {
    xxx <- strsplit(process_result(res)$content, "\n")[[1]]
    out <- paste0(vapply(xxx, function(z) rawToChar(base64enc::base64decode(z)), "",
                  USE.NAMES = FALSE), collapse = "")
  }
  list(repo = repo, exists = exists, out = out)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# res <- get_x_file(repo = "ropensci/fulltext", path = "contents/codemeta.json")
# cat(res$travis)

df <- ro_pkgs()
repos <- gsub("https://github.com/", "", df$packages$url)
out <- lapply(repos, get_x_file, path = "contents/codemeta.json")
res <- lapply(out, function(z) {
  z[vapply(z, is.null, logical(1))] <- NA_character_
  data.frame(z, stringsAsFactors = FALSE)
})
tdata <- tbl_df(bind_rows(res))

## those with x file
tdata %>% filter(exists) %>% .$repo

## those without x file
tdata %>% filter(!exists) %>% .$repo %>% sort %>% cat(sep = "\n")
