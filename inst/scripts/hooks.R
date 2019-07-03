library(dplyr)
library(tibble)

gh_hooks <- function(repo, token = NULL, ...) {
  token <- get_token(token)
  req <- httr::GET(make_url(repo, "hooks"), token, ...)
  out <- process_result(req)
  if (length(out) == 0) return(data.frame(NULL))
  out <- out[sapply(out, function(x) !identical(x, list()))]
  dplyr::bind_rows(out)
}


#gh_hooks(repo = "ropensci/taxize")

url <- "https://raw.githubusercontent.com/ropensci/roregistry/gh-pages/registry.json"
df <- jsonlite::fromJSON(url)
# remove some
df$packages <- tbl_df(df$packages) %>% filter(!name %in% c("sheetseeR", "zenodo"))
urls <- grep("https://github.com", df$packages$url, value = TRUE)
urls <- gsub("https://github.com/", "", urls)
urls <- grep("ropensci|ropenscilabs", urls, value = TRUE)
alldat <- list()
for (i in seq_along(urls)) {
  cat("doing", urls[i], "\n")
  alldat[[i]] <- gh_hooks(urls[i])
}
alldat2 <- stats::setNames(alldat, urls)
alldatdf <- tbl_df(bind_rows(alldat2, .id = "repo"))

# no slack hooks
tmp <- vapply(split(alldatdf, alldatdf$repo), function(z) any(grepl("slack", z$config.url)), TRUE)
df <- data_frame(repo = names(tmp), with_slack = unname(tmp))
df %>% filter(!with_slack) %>% data.frame %>% .$repo %>% cat(sep = "\n")

# alldatdf %>%
#   filter(!grepl("slack", config.url)) %>%
#   select(repo, config.url) %>%
#   distinct() %>% .$repo %>% cat(sep = "\n")

# no hooks at all
alldat2
names(alldat2[vapply(alldat2, NROW, 1) == 0])
