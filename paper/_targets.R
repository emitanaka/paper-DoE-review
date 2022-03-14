library(targets)
library(ggplot2)
library(tarchetypes)
source("R/functions.R")
tar_option_set(packages = c("tidyverse", "rvest", "lubridate",
                            "cranlogs", "glue", "scales", "colorspace",
                            "tidytext", "pluralize", "kableExtra", 
                            "igraph", "ggraph", "ggrepel", "patchwork",
                            "ggwordcloud"))

plot_setup()

list(
  tar_target(doe_pkgs,  ctv_list("ExperimentalDesign")),
  tar_target(npkgs, length(doe_pkgs)),
  tar_target(doe_db, pkg_db(doe_pkgs)),
  tar_target(cran_download_data2, cran_downloads_duration(doe_pkgs, 2)),
  tar_target(cran_download_data5, cran_downloads_duration(doe_pkgs, 5)),
  tar_target(cran_rank2, cran_downloads_rank(cran_download_data2)),
  tar_target(cran_rank5, cran_downloads_rank(cran_download_data5)),
  tar_target(ntop, 8),
  tar_target(top2yrs, cran_rank2$package[1:ntop]),
  tar_target(top5yrs, cran_rank5$package[1:ntop]),
  tar_target(updates2, pkg_updates(top2yrs, duration = 5)),
  tar_target(plot_download_trend, download_trend(cran_rank5, updates2)),
  tar_render(report, "paper.Rmd")
)