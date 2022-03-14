
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
  end <- Sys.Date() - 3 
  start <- end - years(duration)
  cran_downloads(pkgs, from = start, to = end)  
}

cran_downloads_rank <- function(data) {
  data %>% 
    group_by(package) %>% 
    summarise(total = sum(count)) %>% 
    arrange(desc(total)) %>% 
    mutate(rank = 1:n())
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
    filter(update >= Sys.Date() - years(duration))
}


download_trend <- function(datsum, updates) {
  top_pkgs <- unique(updates$package)
  datsum %>% 
    filter(package %in% top_pkgs) %>% 
    mutate(package = factor(package, levels = top_pkgs)) %>%
    ggplot(aes(date, count + 1)) +
    geom_line(data = rename(datsum, pkg = package),
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

