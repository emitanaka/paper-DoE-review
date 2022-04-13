
plot_setup <- function() {
  theme_set(
    theme(panel.background = element_rect(fill = NA),
          panel.grid = element_line(color = "lightgray"),
          axis.text = element_text(color = "black"),
          axis.line = element_line(color = "black", size = 0.7),
          axis.ticks.length = unit(1.4, "mm"),
          axis.ticks = element_line(color = "black", size = 0.7),
          axis.title = element_text(color = "black", face = "bold"),
          strip.background = element_rect(color = "black",
                                          fill = "white"),
          strip.text = element_text(color = "black", 
                                    margin = margin(5, 5, 5, 5)),
          plot.title.position = "plot",
          plot.title = element_text(color = "black", hjust = 0)))
}




ctv_list <- function(ctv) {
  ctv:::.get_pkgs_from_ctv_or_repos(ctv, 
                                    repos = "http://cran.rstudio.com/")[[1]]  
}

cran_downloads_duration <- function(pkgs, duration) {
  end <- as.Date("2022-03-01")  
  start <- end - years(duration)
  cran_downloads(pkgs, from = start, to = end)  
}

cran_ed_scrub_duration <- function(duration) {
  end <- as.Date("2022-03-01")  
  start <- end - years(duration)
  cranscrub::ctvExperimentalDesign %>% 
    rename(count = n_unique) %>% 
    select(-n_total) %>% 
    dplyr::filter(between(date, start, end))
}

cran_downloads_rank <- function(data) {
  data %>% 
    group_by(package) %>% 
    summarise(total = sum(count)) %>% 
    arrange(desc(total)) %>% 
    mutate(rank = 1:n())
}

cran_downloads_rank_by_year <- function(data) {
  data %>% 
    mutate(year = year(date)) %>% 
    group_by(package, year) %>% 
    summarise(total = sum(count)) %>% 
    arrange(desc(total), year) %>% 
    group_by(year) %>% 
    mutate(rank = 1:n()) %>% 
    ungroup()
}

plot_stl_model <- function(data, pkg, updates, trend, speriod1, speriod2) {
  data %>% 
    filter(package == pkg) %>% 
    as_tsibble(index = date) %>% 
    model(STL(count ~ trend(window = !!trend) + season(period = !!speriod1) + season(period = !!speriod2),
              robust = TRUE)) %>% 
    components() %>% 
    autoplot() + 
    geom_vline(data = filter(updates, package == pkg), 
               aes(xintercept = update), color = "red")
  
}

plot_download_distribution <- function(data) {
  data %>% 
    mutate(year = as.factor(year)) %>% 
    ggplot(aes(x = year, y = total)) + 
    geom_violin() +
    geom_boxplot(width = 0.1) + 
    labs(x = "Year", y = "Downloads") + 
    scale_y_log10(label = comma) 
}


pkg_updates <- function(pkgs, duration) {
  pkg_url <- "https://cran.r-project.org/web/packages/{pkg}/index.html"
  pkg_archive <- "https://cran.r-project.org/src/contrib/Archive/{pkg}/"
  res <- map(pkgs, function(pkg) {
    last_update <- read_html(glue(pkg_url)) %>% 
      html_table() %>% 
      .[[1]] %>% 
      filter(X1=="Published:") %>% 
      pull(X2) %>% 
      ymd()
    
    archive_dates <- tryCatch({ 
      read_html(glue(pkg_archive)) %>% 
        html_table() %>%
        .[[1]] %>% 
        pull(`Last modified`) %>% 
        ymd_hm() %>% 
        na.omit() %>% 
        as.Date()
    }, error = function(e) {
      NULL
    })
  c(archive_dates, last_update)
  })
  names(res) <- pkgs
  
  unlist(res) %>% 
    enframe("package", "update") %>% 
    # unlist converts date to integers
    mutate(update = as.Date(update, origin = "1970-01-01"),
           # need to get rid of the numbers appended to pkg names
           package = str_extract(package, paste0(pkgs, collapse="|")),
           package = factor(package, levels = pkgs)) %>% 
    filter(update >= as.Date("2022-03-01") - years(duration))
}


download_trend <- function(data, updates) {
  top_pkgs <- unique(updates$package)
  data %>% 
    filter(package %in% top_pkgs) %>% 
    mutate(package = factor(package, levels = top_pkgs)) %>%
    ggplot(aes(date, count + 1)) +
    geom_line(data = rename(data, pkg = package),
              color = "grey80",
              aes(group = pkg)) +
    geom_line(data = ~rename(., pkg = package),
              color = "grey60",
              aes(group = pkg)) +
    geom_vline(data = updates, 
               aes(xintercept = update),
               color = "red", linetype = "dashed") +
    geom_line() +
    scale_y_log10() +
    scale_x_date(expand = c(0, 0)) + 
    facet_grid(package ~ .) +
    theme(legend.position = "bottom")
}

pkg_db <- function(pkgs) {
  url <- "http://cran.rstudio.com/web/packages/packages.rds"
  db <- readRDS(url(url)) %>% 
    as.data.frame()
  
  db %>% 
    filter(Package %in% pkgs) %>% 
    mutate(Description = str_replace_all(Description, "\n", " "),
           Description = str_squish(Description),
           Title = str_replace_all(Title, "\n", " "))  
}

