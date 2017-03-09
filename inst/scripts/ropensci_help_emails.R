library(gmailr)
qry <- "%s to:(myrmecocystus@gmail.com) -from:(myrmecocystus@gmail.com) -from:(schamber@rice.edu) -from:(info@ropensci.org) -from:(wordpress@ropensci.org) -{scholar, todoist, cran, discuss, f1000, notifications@github.com, appveyor, builds@travis-ci.org, notifications@travis-ci.org, sibbell, r-hub, hipchat, slack, hojoki, do-not-reply@stackexchange.com, appspotmail.com, notifications@helpscout.net, notify@twitter.com, impactstory.org, noreply@youtube.com, plus.google.com, plosone@plos.org, gauges, postmaster.twitter.com, mail@cloze.com, asana.com, noreply@github.com, \"URGENT DISKo FINAL\"} -in:chats"
out <- messages(sprintf(qry, "taxize"))

not_done <- TRUE
page_token <- NULL
out <- list()
i <- 0
while (not_done) {
  i <- i + 1
  tmp <- messages(sprintf(qry, "taxize"), page_token = page_token)
  page_token <- tmp[[1]]$nextPageToken
  out[[i]] <- tmp
  if (is.null(page_token)) not_done <- FALSE
}

# out[[1]]$resultSizeEstimate
# out[[1]]$messages[[1]]

allout <- unlist(lapply(out, function(z) z[[1]]$messages), FALSE)

# all <- lapply(allout, function(z) {
#   message(z$id)
# })
# all

# unique threads
unique(vapply(allout, "[[", "", "threadId"))


do_stuff <- function(package) {
  qry <- "%s to:(myrmecocystus@gmail.com) -from:(alm@plos.org) -from:(kchorn@gmail.com) -from:(myrmecocystus@gmail.com) -from:(schamber@rice.edu) -from:(info@ropensci.org) -from:(wordpress@ropensci.org) -{scholar, todoist, cran, discuss, f1000, notifications@github.com, appveyor, builds@travis-ci.org, notifications@travis-ci.org, sibbell, r-hub, hipchat, slack, hojoki, do-not-reply@stackexchange.com, appspotmail.com, notifications@helpscout.net, notify@twitter.com, impactstory.org, noreply@youtube.com, plus.google.com, plosone@plos.org, gauges, postmaster.twitter.com, mail@cloze.com, asana.com, noreply@github.com, \"URGENT DISKo FINAL\", publons.com, notifications@disqus.net, noreply@medium.com, lagotto.io, elifesciences.org, info@twitter.com, meetup.com, sci.scientific-direct.net, changelog.com, facebookmail.com, blossom.io, ifttt.com, easytobook.com, esa_journals} -in:chats"
  not_done <- TRUE
  page_token <- NULL
  out <- list()
  i <- 0
  while (not_done) {
    i <- i + 1
    tmp <- messages(sprintf(qry, package), page_token = page_token)
    page_token <- tmp[[1]]$nextPageToken
    out[[i]] <- tmp
    if (is.null(page_token)) not_done <- FALSE
  }

  allout <- unlist(lapply(out, function(z) z[[1]]$messages), FALSE)
  list(
    package = package,
    unique_threads = length(unique(vapply(allout, "[[", "", "threadId"))),
    data = out
  )
}

# testing one at a time
vv <- do_stuff("rgbif")
vv$unique_threads
message(vv$data[[1]][[1]]$messages[[1]]$id)

# all packages
library(ropkgs)
library(dplyr)
pkgsdf <- ro_pkgs()
scott_pkgs <- tbl_df(pkgsdf$packages) %>% filter(maintainer == "Scott Chamberlain")
pkg_names <- scott_pkgs$name

results <- lapply(pkg_names, do_stuff)
length(results)
results <- stats::setNames(results, pkg_names)
resultsdf <- tbl_df(data.frame(
  pkg = names(results),
  total = unname(vapply(results, "[[", 1, "unique_threads")),
  stringsAsFactors = FALSE))
resultsdf %>% arrange(desc(total))
# remove ones that are a PITA
tot_estimate <- resultsdf %>%
  filter(!pkg %in% c("traits", "taxa", "bold", "alm")) %>%
  summarise(total = sum(total))


