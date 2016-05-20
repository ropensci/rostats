# github_commits <- function(x) {
#   gcomm <- system.file("extdata", "github_commits.csv", package = "rostats")
#   dat <- read.csv(gcomm)[,-1]
#   dat$date <- as.Date(dat$date)
#   return(dat)
# }
