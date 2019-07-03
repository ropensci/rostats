url <- "https://raw.githubusercontent.com/ropensci/roregistry/gh-pages/registry.json"
df <- jsonlite::fromJSON(url)
library(dplyr)
library(crul)
cli <- crul::HttpClient$new(url = "https://cran.rstudio.com")
nms <- tbl_df(df$packages) %>%
  filter(!on_cran) %>%
  .$name
stats <- vapply(nms, function(z) cli$get(paste0("/web/packages", z))$status_code, 1)
# check any not 404
any(stats != 404)
