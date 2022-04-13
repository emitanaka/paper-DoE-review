library(targets)
library(ggplot2)
library(tarchetypes)
source("R/functions.R")
tar_option_set(packages = c("tidyverse", "rvest", "lubridate",
                            "cranlogs", "glue", "scales", "colorspace",
                            "tidytext", "pluralize", "kableExtra", 
                            "igraph", "ggraph", "ggrepel", "patchwork",
                            "ggwordcloud", "fable", "feasts", "lubridate"))

plot_setup()

list(
  tar_target(doe_pkgs,  ctv_list("ExperimentalDesign")),
  tar_target(npkgs, length(doe_pkgs)),
  tar_target(doe_db, pkg_db(doe_pkgs)),
  tar_target(cran_download_data2, cran_downloads_duration(doe_pkgs, 2)),
  tar_target(cran_download_data5, cran_downloads_duration(doe_pkgs, 5)),
  tar_target(cran_scrub_data2, cran_ed_scrub_duration(2)),
  tar_target(cran_scrub_data5, cran_ed_scrub_duration(5)),
  tar_target(cran_rank2, cran_downloads_rank(cran_download_data2)),
  tar_target(cran_rank5, cran_downloads_rank(cran_download_data5)),
  tar_target(cran_rank_yearly, cran_downloads_rank_by_year(cran_download_data5)),
  tar_target(cran_srank2, cran_downloads_rank(cran_scrub_data2)),
  tar_target(cran_srank5, cran_downloads_rank(cran_scrub_data5)),
  tar_target(cran_srank_yearly, cran_downloads_rank_by_year(cran_scrub_data5)),
  tar_target(ntop, 8),
  tar_target(top2yrs, cran_rank2$package[1:ntop]),
  tar_target(top5yrs, cran_rank5$package[1:ntop]),
  tar_target(updates2, pkg_updates(top2yrs, duration = 5)),
  tar_target(updates_all, pkg_updates(doe_pkgs, duration = Inf)),
  tar_target(plot_download_dist, plot_download_distribution(cran_srank_yearly)),
  tar_target(plot_download_trend, download_trend(cran_download_data5, updates2)),
  tar_target(plot_scrub_trend, download_trend(cran_scrub_data5, updates2)),
  tar_target(plot_stl_7wy, plot_stl_model(cran_scrub_data5, "agricolae", updates2, 7, "week", "year")),

  tar_render(report, "paper.Rmd")
)