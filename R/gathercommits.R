#' Get rOpenSci commits
#'
#' @export
#' @param x One or more R packages, of the form \code{ropensci/rfishbase},
#' case sensitive
#' @param file A date, or other character string to make the file
#' ending unique. Default: \code{Sys.Date()}
#' @param token A GitHub Personal Access Token (PAT). If not given,
#' we look in your env vars for GITHUB_PAT
#' @return writes a data.frame to \code{rappdirs::user_cache_dir("rostats")},
#' prints message of path to file, and returns file path itself
#' @examples \dontrun{
#' library("dplyr")
#' library("ropkgs")
#' library("rostats")
#' pkgs <- ropensci_pkgs()
#'
#'
#' library("dplyr")
#' url <- "https://raw.githubusercontent.com/ropensci/roregistry/gh-pages/registry.json"
#' df <- jsonlite::fromJSON(url)
#' (pkgs <- tbl_df(df$packages) %>%
#'   # filter(on_cran, !cran_archived) %>%
#'   # filter(name != "pleiades") %>%
#'   .$url %>%
#'   gsub('https://github.com/', '', .)
#' )
#'
#' pkgs[3] <- "GuangchuangYu/treeio"
#'
#' z <- gather_commits(x = pkgs)
#' # gather_commits(x = pkgs$owner_repo)
#' }
gather_commits <- function(x, file = Sys.Date(), token = NULL) {
  gh_commits_count_safe <- plyr::failwith(NULL, gh_commits_count)
  out <- lapply(x, gh_commits_count_safe, token = token)
  out2 <- Map(function(a, b) {
    if (NROW(a) == 0) {
      NULL
    } else {
      a$pkgname <- b
      a
    }
  }, out, x)
  out2 <- ct(out2)
  df <- dplyr::bind_rows(out2)
  outbrief <- stats::setNames(df[, c('author.login', 'commit.committer.date',
                              'pkgname')], c("author", "date", "pkg"))
  dir <- file.path(rappdirs::user_cache_dir("R/rostats"), "commits")
  if (!file.exists(dir)) dir.create(dir, recursive = TRUE)
  ff <- file.path(dir, paste0("github_commits_", file, ".csv"))
  utils::write.csv(outbrief, file = ff, row.names = FALSE)
  message("data written to ", ff)
  return(ff)
}
