#!/usr/bin/env RScript

args <- commandArgs(trailingOnly = TRUE)
out_file_name <- args[[1]]
out_file_path <- args[[2]]
rirods_source <- args[[3]]
rirods_version <- args[[4]]
host_name <- args[[5]]

#  files
file_nms <- list.files("data", pattern = "_commits|_repos", full.names = TRUE)
pts <- Map(normalizePath, file_nms)
file_nms <- Map(basename, file_nms)

# run in temp dir
tmp_dir <- tempdir()
setwd(tmp_dir)
print(getwd())

# project
if (!requireNamespace("renv", quietly = TRUE))
  install.packages("renv")

if (!requireNamespace("usethis", quietly = TRUE))
  install.packages("usethis")

usethis::create_project(".", open = FALSE)

# isolate library
renv::init(bare = TRUE, load = FALSE, restart = FALSE)

# install dependencies
install.packages(c("bench", "readr", "knitr", "here", "dplyr",
                   "ggplot2", "readr", "tidyr", "xml2", "jsonlite"))

# install rirods
packageurl <- paste0(rirods_source, rirods_version, ".tar.gz")
install.packages(packageurl, repos = NULL, type = "source")

library("rirods")
use_irods_demo()

# irods project in temp dir
create_irods(host_name, "/tempZone/home")
iauth("rods", "rods")

timer_files <- function(file_name) {

  for_comparison <- readr::read_csv(file_name, show_col_types = FALSE)

  # time execution
  time_iput <- bench::mark(
    iput(file_name, basename(file_name), overwrite = TRUE),
    max_iterations = 100,
    filter_gc = FALSE
  )

  # memory used
  time_iput$mem_alloc <- bench::as_bench_bytes(time_iput$mem_alloc)
  time_iput$source <- paste0(rirods_source, rirods_version)
  time_iput$`file name` <- tools::file_path_sans_ext(basename(file_name))
  time_iput$size <- file.size(file_name)

  # time execution
  time_iget <- bench::mark(
    iget(basename(file_name), basename(file_name), overwrite = TRUE),
    max_iterations = 100,
    filter_gc = FALSE
  )

  # check whether input is same as output
  new_file <- readr::read_csv(basename(file_name), show_col_types = FALSE)
  stopifnot(
    "Files are not the same after downloading from iRODS." = all.equal(for_comparison, new_file)
  )

  # memory used
  time_iget$mem_alloc <- bench::as_bench_bytes(time_iget$mem_alloc)
  time_iget$source <- paste0(rirods_source, rirods_version)
  time_iget$`file name` <- tools::file_path_sans_ext(basename(file_name))
  time_iget$size <- file.size(file_name)
  rbind(time_iget, time_iput)
}

# map over different file sizes with iput and iget commands
timer_sizes <- invisible(Map(timer_files, pts))

# clean-up
invisible(Map(function(x) {irm(x, force = TRUE)}, file_nms))

#output log
readr::write_csv(
  Reduce(rbind, timer_sizes),
  file.path(out_file_path, paste0(out_file_name, "-files.csv"))
)
