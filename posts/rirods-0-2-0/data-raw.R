library(gh)
library(dplyr)
library(purrr)
library(readr)

gh::gh_whoami()

get_repos <- function(org) {

  repos_names <- repos_owner <- repos_private <- repos_fork <- repos_archived <- repos_created_at <-
    repos_fork_count <- repos_open_issues <- repos_stargazers_count <- repos_watchers_count <-
    repos_stargazers_url <- repos_contributors_url  <- repos_commits_url <-
    NULL
  page <- 1
  geht <- TRUE

  while (geht) {

    repos <- try(gh("/orgs/:org/repos", org = org, page = page))

    geht <- any(repos != "")

    if (geht) {
      repos_names <-
        c(repos_names, vapply(repos, "[[", character(1), "name"))
      repos_owner <-
        c(repos_owner, vapply(lapply(repos, "[[", "owner"), "[[", character(1), "login"))
      repos_private <-
        c(repos_private, vapply(repos, "[[", logical(1), "private"))
      repos_fork <-
        c(repos_fork, vapply(repos, "[[", logical(1), "fork"))
      repos_archived <-
        c(repos_archived, vapply(repos, "[[", logical(1), "archived"))
      repos_created_at <-
        c(repos_created_at, vapply(repos, "[[", character(1), "created_at"))
      repos_fork_count <-
        c(repos_fork_count, vapply(repos, "[[", integer(1), "forks"))
      repos_open_issues <-
        c(repos_open_issues, vapply(repos, "[[", integer(1), "open_issues"))
      repos_stargazers_count <-
        c(repos_stargazers_count, vapply(repos, "[[", integer(1), "stargazers_count"))
      repos_watchers_count <-
        c(repos_watchers_count, vapply(repos, "[[", integer(1), "watchers_count"))
      repos_stargazers_url <-
        c(repos_stargazers_url, vapply(repos, "[[", character(1), "stargazers_url"))
      repos_contributors_url <-
        c(repos_contributors_url, vapply(repos, "[[", character(1), "contributors_url"))
      repos_commits_url <-
        c(repos_commits_url, vapply(repos, "[[", character(1), "commits_url"))
      page <- page + 1
    }
  }

  repos_commits_url <-
    vapply(repos_commits_url, function(x)
      gsub("{/sha}", "", x, fixed = TRUE) , character(1))

  tibble(
    names = repos_names,
    owner = repos_owner,
    private = repos_private,
    fork = repos_fork,
    archived = repos_archived,
    created_at = repos_created_at,
    fork_count = repos_fork_count,
    open_issues = repos_open_issues,
    stargazers_count = repos_stargazers_count,
    watchers_count = repos_watchers_count,
    stars_url = repos_stargazers_url,
    contributors_url = repos_contributors_url,
    commits_url = repos_commits_url,
    downloaded_at = format(Sys.time(), '%d %B, %Y')
  )
}

irods_repos <- get_repos(org = "irods")
write_csv(irods_repos, "posts/rirods-0-2-0/data/irods_repos.csv")

tidy_repos <- get_repos(org = "tidyverse")
write_csv(tidy_repos, "posts/rirods-0-2-0/data/tidyverse_repos.csv")

nodejs_repos <- get_repos(org = "nodejs")
write_csv(nodejs_repos, "posts/rirods-0-2-0/data/nodejs_repos.csv")

git_repos <- get_repos(org = "git")
write_csv(git_repos, "posts/rirods-0-2-0/data/git_repos.csv")

get_commits <- function(repos_commits_url) {

  repos_commits_  <- NULL

  for(i in  seq_along(repos_commits_url)) {

    commits_info <- NULL
    page_commits <- 1
    geht_commits <- TRUE

    while (geht_commits) {

      commits <- try(gh(repos_commits_url[i], page = page_commits), silent = TRUE)

      geht_commits <- inherits(commits, "list") & any(commits != "")

      if (geht_commits) {
        commits_info_ <- try(vapply(lapply(lapply(commits, "[[", "commit"), "[[", "author"), "[[", character(1), "name"))
        commits_info_ <- tibble(commits_url = repos_commits_url[i], commits_names = commits_info_)
        # save per page per url
        commits_info <- bind_rows(commits_info, commits_info_)
      }
      page_commits <- page_commits + 1
    }
    # save per url
    repos_commits_ <- bind_rows(repos_commits_, commits_info)
  }
  repos_commits_
}

irods_commits <- get_commits(repos_commits_url = irods_repos$commits_url)
write_csv(irods_commits, "posts/rirods-0-2-0/data/irods_commits.csv")

tidy_commits <- get_commits(repos_commits_url = tidy_repos$commits_url)
write_csv(tidy_commits, "posts/rirods-0-2-0/data/tidy_commits.csv")

nodejs_commits <- get_commits(repos_commits_url = nodejs_repos$commits_url)
write_csv(nodejs_commits, "posts/rirods-0-2-0/data/nodejs_commits.csv")

git_commits <- get_commits(repos_commits_url = git_repos$commits_url)
write_csv(git_commits, "posts/rirods-0-2-0/data/git_commits.csv")
