library(readr)
library(dplyr)
library(forcats)
library(ggplot2)
irods_files <- c("irods_repos.csv", "irods_stargazers.csv", "irods_contributors.csv",
                 "irods_commits.csv")
pt <- here("posts", "welcome", "data", irods_files)

stars <- read_csv(pt[2], show_col_types = FALSE)
contributors <- read_csv(pt[3], show_col_types = FALSE)

repos |>
  filter(!fork) |>
  arrange(desc(stargazers_count)) |>
  mutate(
    names =
      if_else(stargazers_count > stargazers_count[13], names, "other"),
    created_at =
      if_else(stargazers_count > stargazers_count[13], created_at, Sys.time())
  ) |>
  group_by(names) |>
  summarise(
    names = unique(names),
    stars = sum(stargazers_count),
    created_at = unique(created_at)
  ) |>
  ggplot(aes(x = fct_reorder(names, created_at), y = stars)) +
  geom_bar(stat = "identity") +
  coord_flip()
