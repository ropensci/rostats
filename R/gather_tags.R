#' Gather tags
#'
#' @param x a github repository in the form \code{owner/repo},
#' e.g,. \code{ropensci/rfisheries}
#' @param token A GitHub PAT - can just pull your stored PAT from \code{GITHUB_PAT}
#' env var
#' @examples \dontrun{
#' pkgs <- ropensci_pkgs()
#'
#' # get tags
#' gather_tags(pkgs$owner_repo[[15]])
#'
#' # get sha of lastest commit
#' last_sha(pkgs$owner_repo[[15]])
#'
#' # get files
#' gather_files(pkgs$owner_repo[[15]])
#'
#' # get latest release
#' gather_release(pkgs$owner_repo[[15]])
#'
#' # get maintainer and their emails
#' library("ropkgs")
#' library("dplyr")
#' df <- ro_pkgs()
#'
#' # do analysis
#' ## get cran data
#' pkgnames <- df$packages %>%
#'   filter(on_cran) %>%
#'   .$name
#' res <- gather_crans(pkgnames)
#' crandat <- dplyr::bind_rows(res) %>%
#'    dplyr::rename(version = name, name = pkg, version_date = date)
#'
#' ## get github data
#' github_dat <- lapply(pkgs$owner_repo, function(x) {
#'   tags <- gather_tags(x)
#'   files <- gather_files(x)
#'   data.frame(pkg = strsplit(x, "/")[[1]][2],
#'              owner_repo = x,
#'              no_tags = NROW(tags),
#'              latest_tag = if (inherits(tags, "data.frame")) tags[1, "name"] else NA,
#'              has_news = any(grepl("NEWS", files$path)),
#'              release = gather_release(x),
#'              stringsAsFactors = FALSE)
#' })
#'
#' out <- tbl_df(bind_rows(github_dat)) %>%
#'   dplyr::rename(name = pkg) %>%
#'   left_join(df$packages %>% select(name, maintainer, email, on_cran)) %>%
#'   left_join(crandat %>% group_by(name) %>% top_n(1) %>% ungroup())
#'
#' # on CRAN, but no news file
#' out %>%
#'   filter(on_cran, !has_news)
#'
#' # on CRAN, but no git tags
#' out %>%
#'   filter(on_cran, no_tags == 0)
#'
#' # on CRAN, git tags, but no releases
#' out %>%
#'   filter(on_cran, no_tags > 0, nchar(release) == 0) %>% arrange(has_news)
#'
#' # does the most recent CRAN version have a github repo release
#' ## github releases ahead of CRAN verions should be fine
#' ## it's CRAN versions ahead of github releases that's of concern
#' out %>%
#'   filter(on_cran, version != "archived", !is.na(latest_tag)) %>%
#'   select(name, latest_tag, version) %>%
#'   mutate(latest_tag = gsub("[A-Za-z]|-|\\.0$|\\.0\\.0$", "", latest_tag)) %>%
#'   mutate(version = gsub("\\.0$|\\.0\\.0$", "", version)) %>%
#'   mutate(matches = latest_tag == version) %>%
#'   arrange(matches) %>%
#'   filter(!matches) %>%
#'   left_join(out %>% select(name, maintainer, email))
#' }

gather_tags <- function(x, token = NULL) {
  token <- get_token(token)
  req <- httr::GET(sprintf("https://api.github.com/repos/%s/tags", x), token)
  jsonlite::fromJSON(httr::content(req, "text", encoding = "UTF-8"))
}

#' @export
#' @rdname gather_tags
gather_files <- function(x, token = NULL) {
  token <- get_token(token)
  req <- httr::GET(sprintf("https://api.github.com/repos/%s/git/trees/%s", x, last_sha(x)),
                   query = list(per_page = 1), token)
  jsonlite::fromJSON(httr::content(req, "text", encoding = "UTF-8"))$tree
}

#' @export
#' @rdname gather_tags
gather_release <- function(x, token = NULL) {
  token <- get_token(token)
  req <- httr::GET(sprintf("https://api.github.com/repos/%s/releases", x),
                   query = list(per_page = 1), token)
  jsonlite::fromJSON(httr::content(req, "text", encoding = "UTF-8"))$body %||% ''
}

# helpers ------------------------------
last_sha <- function(x, token = NULL) {
  token <- get_token(token)
  req <- httr::GET(sprintf("https://api.github.com/repos/%s/commits", x),
                   query = list(per_page = 1), token)
  jsonlite::fromJSON(httr::content(req, "text", encoding = "UTF-8"))$sha
}
