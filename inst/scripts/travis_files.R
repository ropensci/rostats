# "GET /repos/:owner/:repo/contents/:path"

get_travis_file <- function(repo, token = NULL, ...) {
  token <- get_token(token)
  res <- httr::GET(make_url(repo, "contents/.travis.yml"), token, ...)
  exists <- TRUE
  travis <- NULL
  if (res$status_code != 200) {
    exists <- res$status_code
  } else {
    xxx <- strsplit(process_result(res)$content, "\n")[[1]]
    travis <- paste0(vapply(xxx, function(z) rawToChar(base64enc::base64decode(z)), "",
                  USE.NAMES = FALSE), collapse = "")
  }
  list(repo = repo, exists = exists, travis = travis)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# res <- get_travis_file("ropensci/rgbif")
# cat(res$travis)

library("ropkgs")
library("dplyr")
df <- ro_pkgs()
repos <- gsub("https://github.com/", "", df$packages$url)
out <- lapply(repos, get_travis_file)
res <- lapply(out, function(z) {
  if (!is.null(z$travis)) {
    z$travis <- yaml::yaml.load(z$travis)
    z$travis_bits <-
      data.frame(package = strsplit(z$repo, "/")[[1]][2],
                 language = z$travis$language,
                 sudo = z$travis$sudo %||% NA_character_,
                 stringsAsFactors = FALSE)
    z$travis_bits$sudo <- as.character(z$travis_bits$sudo)
  } else {
    z$travis_bits <-
      data.frame(package = strsplit(z$repo, "/")[[1]][2],
                 language = NA,
                 sudo = NA_character_,
                 stringsAsFactors = FALSE)
  }
  return(z)
})
tdata <- bind_rows(lapply(res, "[[", "travis_bits"))

# bind to registry data
allres <- tbl_df(full_join(tdata, df$packages, by = c('package' = 'name')))
allres

# those that are lang R and are using sudo=required && scott's
allres %>%
  filter(language == "r", sudo == "required") %>%
  filter(maintainer == "Scott Chamberlain")

# others
allres %>%
  filter(language == "r", sudo == "required") %>%
  filter(maintainer != "Scott Chamberlain") %>%
  data.frame

# sudo NA
allres %>%
  filter(language == "r", is.na(sudo)) %>% data.frame

# language not "r"
allres %>%
  filter(language != "r") %>% data.frame
