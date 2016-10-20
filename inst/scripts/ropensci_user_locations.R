library(dplyr)
library(ggplot2)

dat <- readr::read_csv("/Users/sacmac/Library/Caches/rostats/commits/github_commits_2016-10-14.csv")

users <- sort(na.omit(unique(dat$author)))

`%||%` <- function(x, y) if (is.null(x)) y else x

out <- lapply(users, function(z) {
  res <- tryCatch(gh_user(z), error = function(e) e)
  if (inherits(res, "error")) {
    tibble::data_frame()
  } else {
    tibble::data_frame(user = z, location = res$location %||% NA)
  }
})

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
dat <- readr::read_csv("~/countries.csv")
dat %>%
  group_by(country2) %>%
  tally() %>%
  arrange(desc(n)) %>%
  ggplot(., aes(reorder(country2, n), n)) + geom_bar(stat = "identity") + coord_flip()
