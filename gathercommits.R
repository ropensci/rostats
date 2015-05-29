library("httr")
library("ropkgs")
library("plyr")
library("dplyr")

source("github_api_functions.R")

## get ropensci packages, actual R packages, via the ropensci registry
df <- ropkgs::ro_pkgs()
pkgs <- df$packages$name

## safe version that gives back NULL
gh_commits_count_safe <- plyr::failwith(NULL, gh_commits_count)
out <- lapply(pkgs, gh_commits_count_safe)
out2 <- Map(function(x, y) {
  if (NROW(x) == 0) {
    NULL
  } else {
    x$pkgname <- y
    x
  }
}, out, pkgs)
out2 <- ct(out2)
df <- rbind_all(out2)
outbrief <- setNames(df[, c('author.login', 'commit.committer.date', 'pkgname')],
                     c("author", "date", "pkg"))
write.csv(outbrief, file = "data/github_commits.csv")
