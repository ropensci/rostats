# gh_hooks(repo = "ropensci/taxize")
#
# gh_hooks <- function(repo, token = NULL, ...) {
#   token <- get_token(token)
#   req <- httr::GET(make_url(repo, "hooks"), token)
#   out <- process_result(req)
#   outout <- outout[sapply(outout, function(x) !identical(x, list()))]
#   dplyr::bind_rows(outout)
# }
dat <- tbl_df(readr::read_csv("inst/extdata/github_commits_2016-05-19.csv"))

df <- dat %>%
  group_by(pkg) %>%
  summarise(
    no_contribs = length(unique(author)),
    no_commits = length(date),
    first_commit = min(date),
    comm_per_contrib = round(no_commits/no_contribs, 0)
  ) %>%
  ungroup()
div <- dat %>%
  group_by(pkg, author) %>%
  summarise(
    no_contribs = length(author)
  ) %>%
  ungroup() %>%
  group_by(pkg) %>%
  summarise(
    diversity = diversity(t(as.matrix(no_contribs)))
  ) %>%
  arrange(desc(diversity))

df <- df %>% left_join(div)

df %>% arrange(desc(no_contribs))
df %>% arrange(desc(no_commits))
df %>% arrange(desc(comm_per_contrib))
df %>% arrange(desc(diversity))
df %>% arrange(first_commit)

ggplot(df, aes(no_contribs, diversity)) +
  geom_point()
