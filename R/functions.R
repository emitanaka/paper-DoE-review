
plot_setup <- function() {
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
          plot.title = element_text(color = "black", hjust = 0))
}

ctv_list <- function(ctv) {
  ctv:::.get_pkgs_from_ctv_or_repos(ctv, 
                                    repos = "http://cran.rstudio.com/")[[1]]  
}

get_pkg_updates <- function(pkgs, message = FALSE) {
  res <- lapply(pkgs, function(pkg) {
    message(paste("Package", pkg))
    pkgsearch::cran_package_history(pkg) %>% 
      select(package = Package, update = date) %>% 
      mutate(update = as.Date(update))
  })
  do.call("rbind", res)
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

cran_downloads_rank_by_year <- function(data, first_release) {
  data %>% 
    mutate(year = year(date)) %>% 
    group_by(package, year) %>% 
    summarise(total = sum(count)) %>% 
    arrange(desc(total), year) %>% 
    left_join(first_release, by = "package") %>% 
    group_by(year) %>% 
    mutate(rank = 1:n()) %>% 
    ungroup() %>% 
    filter(year > year(first)) %>% 
    filter(year != year(Sys.Date()))
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

plot_download_distribution <- function(data, ginidata) {
  data %>% 
    mutate(year = as.factor(year)) %>% 
    ggplot(aes(x = year, y = total)) + 
    geom_violin() +
    geom_boxplot(width = 0.1) + 
    labs(x = "Year", y = "Downloads") + 
    scale_y_log10(label = comma) +
    geom_label(data = mutate(ginidata, year = as.factor(year)), 
              aes(y = 900, label = scales::percent(gini, 0.1)))
}

plot_lorenz_curve <- function(data, year) {
  
  dat <- data %>% 
    filter(year == .env$year) 
    
  data.frame(p = ineq::Lc(dat$total)$p, L = ineq::Lc(dat$total)$L) %>% 
    ggplot(aes(p, L)) +
    geom_point() +
    geom_line() +
    geom_abline()
}

gini_coef <- function(data) {
  dat <- data %>% 
    group_by(year) %>% 
    summarise(gini = ineq::Gini(total)) 
}

plot_rank_distribution <- function(data, nrank = 10, stat) {
  dat <- data %>% 
    mutate(year = as.factor(year)) %>% 
    filter(rank <= nrank)
  g <- ggplot(dat, aes(x = year, y = {{stat}}, color = package, group = package)) + 
    geom_point() + 
    geom_line() +
    labs(x = "Year", y = "Download rank by year") +
    scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", 
                                  "#F0E442", "#0072B2", "#D55E00", 
                                  "#CC79A7", "#000000", "#888888",
                                  "#88CCEE", "#CC6677", "#DDCC77"),
                       breaks = dat %>% 
                         filter(year == 2021) %>% 
                         arrange(desc(total)) %>% 
                         pull(package) %>% 
                         union(dat$package))
  if(rlang::as_string(rlang::ensym(stat))=="rank") {
    g + scale_y_reverse(breaks = 1:nrank)
  } else {
    g + scale_y_log10(label = comma)
  }
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



singularize2 <- function(x) {
  res <- singularize(x)
  res <- ifelse(res=="bia", "bias", res)
  ifelse(res=="baye", "bayes", res)
}

get_ngram <- function(ngram, desc = c("Title", "Description")) {
  stop_words_rex <- c(paste0("^", stop_words$word, "$"), "^doi", "^[0-9.]+$")
  desc <- match.arg(desc)
  words <- paste0("word", 1:ngram)
  doe_db %>% 
    unnest_tokens(word, desc, token = "ngrams", n = ngram) %>% 
    separate(word, words, sep = " ") %>% 
    mutate(across(num_range("word", 1:ngram), singularize2)) %>% 
    distinct(!!!map(c("Package", words), as.name)) %>% 
    # allow stop words in between
    filter(if_all(num_range("word", c(1, ngram)), 
                  ~!str_detect(., paste0(stop_words_rex, collapse = "|")))) %>% 
    count(!!!map(words, as.name), sort = TRUE) %>% 
    mutate(word = do.call("paste", map(words, ~eval(parse(text = .x))))) %>% 
    select(word, n) 
}

pkg_updates_duration <- function(pkgs, duration) {
  pkg_updates(pkgs) %>% 
    filter(update >= as.Date("2022-03-01") - years(duration))
}

combine_dl <- function(ngram_df, desc) {
  dldat_sum %>% 
    left_join(doe_db, by = c("package" = "Package")) %>% 
    select(package, total, {{desc}}) %>% 
    mutate(bigram_title = map({{desc}}, ~filter(ngram_df, 
                                                n > 1,
                                                str_detect(tolower(.x), word)))) %>% 
    unnest_longer(bigram_title) %>% 
    unpack(bigram_title) %>% 
    group_by(word) %>% 
    summarise(n = unique(n),
              pkgs = list(package),
              total = sum(total)) %>% 
    filter(!is.na(word)) %>% 
    arrange(desc(total))
}
