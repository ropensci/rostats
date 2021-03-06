% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gather_tags.R
\name{gather_tags}
\alias{gather_tags}
\alias{gather_files}
\alias{gather_release}
\title{Gather tags}
\usage{
gather_tags(x, token = NULL)

gather_files(x, token = NULL)

gather_release(x, token = NULL)
}
\arguments{
\item{x}{a github repository in the form \code{owner/repo},
e.g,. \code{ropensci/rfisheries}}

\item{token}{A GitHub PAT - can just pull your stored PAT from \code{GITHUB_PAT}
env var}
}
\description{
Gather tags
}
\examples{
\dontrun{
pkgs <- ropensci_pkgs()

# get tags
gather_tags(pkgs$owner_repo[[15]])

# get sha of lastest commit
last_sha(pkgs$owner_repo[[15]])

# get files
gather_files(pkgs$owner_repo[[15]])

# get latest release
gather_release(pkgs$owner_repo[[15]])

# get maintainer and their emails
library("ropkgs")
library("dplyr")
df <- ro_pkgs()

# do analysis
## get cran data
pkgnames <- df$packages \%>\%
  filter(on_cran) \%>\%
  .$name
res <- gather_crans(pkgnames)
crandat <- dplyr::bind_rows(res) \%>\%
   dplyr::rename(version = name, name = pkg, version_date = date)

## get github data
github_dat <- lapply(pkgs$owner_repo, function(x) {
  tags <- gather_tags(x)
  files <- gather_files(x)
  data.frame(pkg = strsplit(x, "/")[[1]][2],
             owner_repo = x,
             no_tags = NROW(tags),
             latest_tag = if (inherits(tags, "data.frame")) tags[1, "name"] else NA,
             has_news = any(grepl("NEWS", files$path)),
             release = gather_release(x),
             stringsAsFactors = FALSE)
})

out <- tbl_df(bind_rows(github_dat)) \%>\%
  dplyr::rename(name = pkg) \%>\%
  left_join(df$packages \%>\% select(name, maintainer, email, on_cran)) \%>\%
  left_join(crandat \%>\% group_by(name) \%>\% top_n(1) \%>\% ungroup())

# on CRAN, but no news file
out \%>\%
  filter(on_cran, !has_news)

# on CRAN, but no git tags
out \%>\%
  filter(on_cran, no_tags == 0)

# on CRAN, git tags, but no releases
out \%>\%
  filter(on_cran, no_tags > 0, nchar(release) == 0) \%>\% arrange(has_news)

# does the most recent CRAN version have a github repo release
## github releases ahead of CRAN verions should be fine
## it's CRAN versions ahead of github releases that's of concern
out \%>\%
  filter(on_cran, version != "archived", !is.na(latest_tag)) \%>\%
  select(name, latest_tag, version) \%>\%
  mutate(latest_tag = gsub("[A-Za-z]|-|\\\\.0$|\\\\.0\\\\.0$", "", latest_tag)) \%>\%
  mutate(version = gsub("\\\\.0$|\\\\.0\\\\.0$", "", version)) \%>\%
  mutate(matches = latest_tag == version) \%>\%
  arrange(matches) \%>\%
  filter(!matches) \%>\%
  left_join(out \%>\% select(name, maintainer, email))
}
}
