## cumulative contributors
library("plyr")
library("dplyr")
library("ggplot2")
library("reshape")
library("scales")

## if using time window, uncomment, and uncomment annotate() call below
# fromdate <- as.Date("2013-05-01")
# todate <- as.Date("2015-05-29")

## read in data
dat <- read.csv("data/github_commits")[,-1]
dat$date <- as.Date(dat$date)
non_core_dat <- tbl_df(dat) %>%
  filter(!author %in% c("sckott", "karthik", "cboettig"))

## summarise
firstdates <- non_core_dat %>%
  select(-pkg) %>%
  group_by(author) %>%
  arrange(date) %>%
  filter(rank(date, ties.method = "first") == 1) %>%
  ungroup() %>%
  mutate(count = 1) %>%
  arrange(date) %>%
  mutate(cumsum = cumsum(count))

## plot
ggplot(firstdates, aes(date, cumsum)) +
  geom_line(size = 2) +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks.x = element_blank(),
        panel.border = element_rect(size = 2)) +
  # annotate("rect", xmin = fromdate, xmax = todate, ymin = 0, ymax = Inf, alpha = 0.2, fill = "blue") +
  scale_x_date(labels = date_format("%Y/%m")) +
  labs(x = 'May 2011 to June 2015', y = 'Cumulative New Contributors')
