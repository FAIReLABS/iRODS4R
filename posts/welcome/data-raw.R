library("gh")
library("dplyr")
library("purrr")

get_repos <- function(org) {

  repos_names <- NULL
  page <- 1
  geht <- TRUE

  while (geht) {

    repos <- try(gh("/orgs/:org/repos", org = org, page = page))

    geht <- any(repos != "")

    if (geht) {
      repos_names <-
        c(repos_names, vapply(repos, "[[", character(1), "name"))
      repos_watchers <-
        c(repos_watchers, vapply(repos, "[[", integer(1), "watchers_count"))
      page <- page + 1
    }
  }

  data.frame(names= repos_names, watchers_n = repos_watchers)
}

irods_repos <- get_repos(org = "irods")
