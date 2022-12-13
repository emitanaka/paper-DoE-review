library(targets)
library(ggplot2)
library(tarchetypes)
source("R/functions.R")
tar_option_set(packages = c("tidyverse", "lubridate", "cranlogs", "glue", 
                            "scales", "tidytext", "pluralize", "patchwork",
                            "ineq", "cranscrub", "rlang", "igraph", "tidygraph"))


list(
  tar_target(ctv_table, cranscrub::ctv_table()),
  tar_target(doe_pkgs,  ctv_pkgs(ctv_table, "ExperimentalDesign")),
  tar_target(doe_npkgs, length(doe_pkgs)),
  tar_target(pkg_db_clean, ctv_pkg_db(ctv_table)),
  # this part takes a long time to complete (~6 hours)
  tar_target(pkg_downloads, pkg_cran_downloads(pkg_db_clean$Package, 
                                               start = 2013, end = 2021)),
  tar_target(pkg_updates, pkg_update_dates(pkg_db_clean$Package)),
  tar_target(ctv_pkg_data, ctv_combine_pkg_data(ctv_table,
                                                pkg_downloads, 
                                                pkg_db_clean,
                                                pkg_updates)),
  tar_target(ctv_rank_yearly, ctv_downloads_rank_by_year(ctv_pkg_data)),
  tar_target(ctv_gini_yearly, ctv_gini_by_year(ctv_rank_yearly)),
  tar_target(ctv_lorenz_yearly, ctv_lorenz_by_year(ctv_rank_yearly)),
  tar_target(doe_bigram_all, ctv_pkg_data %>% 
               filter(name == "ExperimentalDesign") %>% 
               ngram_data(2, "Both")),
  tar_target(doe_bigram_all_with_download_data, 
             ngram_with_download_data(doe_bigram_all, ctv_rank_yearly %>% 
                                        filter(name == "ExperimentalDesign"))),
  tar_target(ctv_summary_table, ctv_summary(ctv_pkg_data)),
  tar_target(doe_pkg_network, ctv_pkg_data %>% 
               filter(name == "ExperimentalDesign") %>% 
               pkg_network()),
  tar_target(access_date_pkg_downloads, 
             as.Date(file.info("_targets/objects/pkg_downloads")$mtime)),
  
  tar_target(save_ctv_pkg_data, save_output(ctv_pkg_data)),
  tar_target(save_ctv_rank_yearly, save_output(ctv_rank_yearly)),
  tar_target(save_ctv_gini_yearly, save_output(ctv_gini_yearly)),
  tar_target(save_ctv_lorenz_yearly, save_output(ctv_lorenz_yearly)),
  tar_target(save_doe_bigram_all_with_download_data, save_output(doe_bigram_all_with_download_data)),
  tar_target(save_ctv_summary_table, save_output(ctv_summary_table)),
  tar_target(save_doe_pkg_network, save_output(doe_pkg_network)),
  tar_target(save_access_date_pkg_downloads, save_output(access_date_pkg_downloads)),

  tar_render(report, "paper.Rmd"),
  tar_target(index, rmarkdown::render("paper.Rmd", output_file = 'index.html')),
  tar_render(supp, "supp.Rmd"),
  tar_render(arxiv, "paper/arxiv/arxiv.Rmd"),
  NULL
)