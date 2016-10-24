library(dplyr)
`%||%` <- function(x, y) if (is.null(x)) y else x

## code contribs - doesn't include people that have opened issues
##   or those that have sent emails with feedback/etc.
##   or those that have engaged on discussion forum
##   etc.
dat <- readr::read_csv("/Users/sacmac/Library/Caches/rostats/commits/github_commits_2016-10-14.csv")
users <- sort(na.omit(unique(dat$author)))

out <- lapply(users, function(z) {
  res <- tryCatch(gh_user(z), error = function(e) e)
  if (inherits(res, "error")) {
    tibble::data_frame()
  } else {
    tibble::data_frame(user = z, location = res$location %||% NA)
  }
})


## discussion forum
##   - no way to do paging with API, so need to download dump of users
forum <- readr::read_csv("")



## find countries for each user - already prepared, rerun again as needed
df <- bind_rows(out)
df <- df %>% filter(!is.na(location))
ctrs <- lapply(df$location, function(w) {
  tmp <- geonames::GNsearch(name = w)
  if (NROW(tmp) == 0 || NROW(tmp) > 1) {
    return(w)
  } else {
    tmp$countryName
  }
})
ctrs[vapply(ctrs, is.null, logical(1))] <- NA
df$country <- unlist(ctrs)
readr::write_csv(df, path = "~/countries.csv")

# here, hand edited country names - could have done programatically, but there wasn't many to do,
#   automate at some point
user_countries <- readr::read_csv("~/countries.csv")

