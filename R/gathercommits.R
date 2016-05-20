#' Get rOpenSci commits
#'
#' @export
#' @param x One or more R packages, of the form \code{ropensci/rfishbase},
#' case sensitive
#' @param file A date, or other character string to make the file
#' ending unique. Default: \code{Sys.Date()}
#' @param token A GitHub Personal Access Token (PAT). If not given,
#' we look in your env vars for GITHUB_PAT
#' @return writes a data.frame to extdata/, returns path to file
#' @examples \dontrun{
#' pkgs <- ropensci_pkgs()
#' gather_commits(x = pkgs$owner_repo[1:10])
#' gather_commits(x = pkgs$owner_repo)
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
  outbrief <- setNames(df[, c('author.login', 'commit.committer.date', 'pkgname')],
                       c("author", "date", "pkg"))
  ff <- paste0("extdata/github_commits_", file, ".csv")
  write.csv(outbrief, file = ff, row.names = FALSE)
  message("data written to ", ff)
}
