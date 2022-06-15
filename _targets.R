library(targets)
library(ggplot2)
library(tarchetypes)
source("R/functions.R")
source("R/plot.R")
tar_option_set(packages = c("tidyverse", "rvest", "lubridate",
                            "cranlogs", "glue", "scales", "colorspace",
                            "tidytext", "pluralize", "kableExtra", 
                            "igraph", "ggraph", "ggrepel", "patchwork",
                            "ggwordcloud", "fable", "feasts", "lubridate",
                            "ineq", "cranscrub", "tidygraph", "ggraph", 
                            "rlang"))


list(
  tar_target(doe_pkgs,  cranscrub::ctv_pkgs("ExperimentalDesign")),
  tar_target(ctv_pkgs_df, cranscrub::ctv_table()),
  tar_target(npkgs, length(doe_pkgs)),
  tar_target(doe_db, cranscrub::pkg_db(doe_pkgs)),
  tar_target(updates_all, pkg_update_dates(doe_pkgs)),
  tar_target(first_release, pkg_first_release(updates_all)),
  
  tar_target(cran_download_data2, cran_downloads_duration(doe_pkgs, 2020)),
  tar_target(cran_download_data5, cran_downloads_duration(doe_pkgs, 2013)),
  tar_target(cran_downloads_all_ctv, map_dfr(1:nrow(ctv_pkgs_df), ~{
      cran_downloads_duration(ctv_pkgs_df$packages[[.x]], 2013) %>% 
        mutate(ctv = ctv_pkgs_df$name[[.x]])
    })),
  tar_target(updates_all_ctv, map_dfr(1:nrow(ctv_pkgs_df), ~{
    pkg_update_dates(ctv_pkgs_df$packages[[.x]])
  })),
  tar_target(first_release_all, pkg_first_release(updates_all_ctv)),
  
  
  
  # scrub data --- not much interesting difference 
  tar_target(cran_scrub_data2, cran_ed_scrub_duration(2)),
  tar_target(cran_scrub_data5, cran_ed_scrub_duration(5)),
  tar_target(cran_srank2, cran_downloads_rank(cran_scrub_data2)),
  tar_target(cran_srank5, cran_downloads_rank(cran_scrub_data5)),
  tar_target(cran_scrub_rank_yearly, cran_downloads_rank_by_year(cran_scrub_data5, first_release)),
  # ########## 
  
  tar_target(cran_rank2, cran_downloads_rank(cran_download_data2)),
  tar_target(cran_rank5, cran_downloads_rank(cran_download_data5)),
  tar_target(cran_rank_yearly, cran_downloads_rank_by_year(cran_download_data5, first_release)),
  tar_target(cran_rank_yearly_all, cran_downloads_rank_by_year_ctv(cran_downloads_all_ctv, first_release_all)),
  
  ## bottom
  # tar_target(bottom2yrs, cran_rank2$package[(nrow(cran_rank2) - 7):nrow(cran_rank2)]),
  # tar_target(updates_duration_bottom, pkg_updates_duration(bottom2yrs, duration = 5)),
  # tar_target(plot_download_trend_bottom, plot_download_trend(cran_download_data5, updates_duration_bottom) + plot_setup()),
  # 
  # ## middle
  # tar_target(middle2yrs, cran_rank2$package[(nrow(cran_rank2)/2 - 7):(nrow(cran_rank2)/2)]),
  # tar_target(updates_duration_middle, pkg_updates_duration(middle2yrs, duration = 5)),
  # tar_target(plot_download_trend_middle, plot_download_trend(cran_download_data5, updates_duration_middle) + plot_setup()),
  # 
  
  ### top
  
  tar_target(ntop, 8),
  tar_target(top2yrs, cran_rank2$package[1:ntop]),
  tar_target(top5yrs, cran_rank5$package[1:ntop]),
  tar_target(updates_duration_5, pkg_updates_duration(top2yrs, duration = 5)),
  tar_target(gini_yearly_ctv, gini_coef_by_ctv(cran_rank_yearly_all)), 
  tar_target(gini_yearly_doe, gini_yearly_ctv %>% 
               filter(ctv == "ExperimentalDesign")),
  tar_target(doe_lorenz2021, cran_rank_yearly_all %>% 
               filter(ctv == "ExperimentalDesign", year == 2021) %>% 
               lorenz_data()), 
  tar_target(plot_lorenz2021, plot_lorenz_curve(doe_lorenz2021)),
  tar_target(plot_download_dist_all, plot_download_distribution(cran_rank_yearly_all, gini_yearly_ctv) + 
               plot_setup() + facet_wrap(~ctv, ncol = 4)),
  tar_target(plot_download_dist, plot_download_distribution(cran_rank_yearly, gini_yearly_doe) + plot_setup()),
  tar_target(plot_rank_dist, plot_rank_distribution(cran_rank_yearly, stat = rank) + plot_setup()),
  tar_target(plot_rank_download_dist, plot_rank_distribution(cran_rank_yearly, stat = total) + plot_setup()),
  tar_target(plot_download_trend_5, plot_download_trend(cran_download_data5, updates_duration_5) + plot_setup()),
  ## scrub data
  tar_target(plot_scrub_download_dist, plot_download_distribution(cran_scrub_rank_yearly, gini_yearly_doe) + plot_setup()),
  tar_target(plot_scrub_rank_dist, plot_rank_distribution(cran_scrub_rank_yearly, stat = rank) + plot_setup()),
  tar_target(plot_scrub_trend, plot_download_trend(cran_scrub_data5, updates_duration_5) + plot_setup()),
  #tar_target(plot_scrub_trend_middle, plot_download_trend(cran_scrub_data5, updates_duration_middle) + plot_setup()),
  #tar_target(plot_scrub_trend_bottom, plot_download_trend(cran_scrub_data5, updates_duration_bottom) + plot_setup()),
  
  #######
  #tar_target(plot_stl_7wy, plot_stl_model(cran_scrub_data5, "agricolae", updates2, 7, "week", "year") + plot_setup()),
  
  tar_target(bigram_title, ngram_data(doe_db, 2, "Title")),
  tar_target(bigram_desc, ngram_data(doe_db, 2, "Description")),
  tar_target(trigram_desc, ngram_data(doe_db, 3, "Description")),
  tar_target(unigram_all, ngram_data(doe_db, 1, "Both")),
  tar_target(bigram_all, ngram_data(doe_db, 2, "Both")),
  tar_target(trigram_all, ngram_data(doe_db, 3, "Both")),
  
  tar_target(ngram_download_data, ngram_with_download_data(rbind(unigram_all, bigram_all, trigram_all), cran_rank_yearly)),

  tar_target(plot_wordcloud2years, plot_word_cloud_over_years(filter(ngram_download_data, ngram == 2))),
  tar_target(plot_wordcloud2, plot_word_cloud(filter(ngram_download_data, ngram == 2, year == 2021))),
  # trigram is not that helpful - very little trigrams across packages (n = 1 or 2)
  tar_target(plot_wordcloud3, plot_word_cloud(filter(ngram_download_data, ngram == 3))),
  
  tar_target(ctv_pkgs_data, ctv_pkgs_df()),
  tar_target(ctv_summary_table, ctv_summary(ctv_pkgs_data)),
  
  tar_target(doe_pkg_network, pkg_network(doe_db)),
  tar_target(plot_doe_network, plot_pkg_network(doe_pkg_network)),
  
  tar_target(plot_survey_network, plot_pkg_network_by_ctv("OfficialStatistics")),
  tar_target(plot_clinical_network, plot_pkg_network_by_ctv("ClinicalTrials")),
  tar_target(plot_reproducible_network, plot_pkg_network_by_ctv("ReproducibleResearch")),
  tar_target(plot_optimization_network, plot_pkg_network_by_ctv("Optimization")),
  tar_target(plot_cluster_network, plot_pkg_network_by_ctv("Cluster")),
  tar_target(plot_ts_network, plot_pkg_network_by_ctv("TimeSeries")),
  
  tar_render(report, "paper.Rmd"),
  tar_render(index, rmarkdown::render("paper.Rmd", output_file = 'index.html')),
  tar_render(supp, "supp.Rmd"),
  tar_render(arxiv, "paper/arxiv/arxiv.Rmd"),
  NULL
)