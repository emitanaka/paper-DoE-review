


ctv_list <- function(ctv) {
  ctv:::.get_pkgs_from_ctv_or_repos(ctv, 
                                    repos = "http://cran.rstudio.com/")[[1]]  
}

lorenz_data <- function(data) {
  data.frame(p = ineq::Lc(data$total)$p, L = ineq::Lc(data$total)$L) 
}

pkg_first_release <- function(data) {
  data %>% 
    group_by(package) %>% 
    summarise(first = min(update)) %>% 
    ungroup()
}

pkg_update_dates <- function(pkgs, message = FALSE) {
  res <- lapply(pkgs, function(pkg) {
    message(paste("Package", pkg))
    pkgsearch::cran_package_history(pkg) %>% 
      select(package = Package, update = date) %>% 
      mutate(update = as.Date(update))
  })
  do.call("rbind", res)
}


cran_downloads_duration <- function(pkgs, start_year) {
  # it can have trouble downloading long periods so break it up by year
  map_dfr(start_year:2021, ~{
    start <- as.Date(paste0(.x, "-01-01"))
    end <- as.Date(paste0(.x, "-12-31"))
    cran_downloads(pkgs, from = start, to = end)  
  })
  
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

cran_downloads_rank_by_year_ctv <- function(data, first_release) {
  data %>% 
    mutate(year = year(date)) %>% 
    group_by(package, year) %>% 
    summarise(total = sum(count),
              ctv = unique(ctv)) %>% 
    arrange(desc(total), year) %>% 
    left_join(first_release, by = "package") %>% 
    group_by(year, ctv) %>% 
    mutate(rank = 1:n()) %>% 
    ungroup() %>% 
    filter(year > year(first)) %>% 
    filter(year != year(Sys.Date()))
}


gini_coef <- function(data) {
  data %>% 
    group_by(year) %>% 
    summarise(gini = ineq::Gini(total),
              n = n()) 
}

gini_coef_by_ctv <- function(data) {
  data %>% 
    group_by(year, ctv) %>% 
    summarise(gini = ineq::Gini(total),
              n = n()) 
}






singularize2 <- function(x) {
  res <- singularize(x)
  res <- ifelse(res=="bia", "bias", res)
  ifelse(res=="baye", "bayes", res)
}

ngram_data <- function(data, ngram, desc = c("Title", "Description", "Both")) {
  stop_words_rex <- c(paste0("^", stop_words$word, "$"), "^doi", "provide", "^[0-9.]+$", "jss[.]v065", "e.g", "uni", "offer", "calculate")
  desc <- match.arg(desc)
  words <- paste0("word", 1:ngram)
  DUMMYWORD <- "xxxdummywordxxx"
  data %>% 
    mutate(Title = tolower(Title),
           Description = tolower(Description),
           # end of title and beginning of description could form a ngram so avoid
           # this with a dummy word
           Both = paste(Title, DUMMYWORD, Description)) %>%
    # n_min = 2 -- decided not to do this and do it separately to make 
    # word separation below easier
    unnest_tokens(word, desc, token = "ngrams", n = ngram) %>% 
    separate(word, words, sep = " ") %>% 
    mutate(across(num_range("word", 1:ngram), singularize2)) %>% 
    distinct(!!!map(c("Package", words), as.name)) %>% 
    # allow stop words in between
    filter(if_all(num_range("word", c(1, ngram)), 
                  ~!str_detect(., paste0(stop_words_rex, collapse = "|")))) %>% 
    mutate(word = do.call("paste", map(words, ~eval(parse(text = .x))))) %>% 
    filter(!str_detect(word, DUMMYWORD)) %>% 
    group_by(word) %>% 
    summarise(n = n(), pkgs = list(Package)) %>% 
    arrange(desc(n)) %>% 
    mutate(ngram = ngram)
}

pkg_updates_duration <- function(pkgs, duration) {
  pkg_updates(pkgs) %>% 
    filter(update >= as.Date("2022-03-01") - years(duration))
}

ngram_with_download_data <- function(ngram_df, download_df) {
  total_downloads <- download_df %>% 
    group_by(year) %>% 
    summarise(TOTAL = sum(total)) %>% 
    deframe()
  
  ngram_df %>% 
    mutate(downloads = map(pkgs, ~ download_df %>% 
                        filter(package %in% .x) %>% 
                        group_by(year) %>% 
                        summarise(total = sum(total)) %>% 
                        ungroup())) %>% 
    unnest_longer(downloads) %>% 
    unpack(downloads) %>% 
    mutate(perc = total / total_downloads[as.character(year)] * 100)
  
}


ctv_pkgs_df <- function(topics = NULL) {
  if(is.null(topics)) {
    imap_dfr(cranscrub::ctv_names(show_topic = TRUE), 
             ~data.frame(name = .y, topic = .x, package = cranscrub::ctv_pkgs(.y)))
  } else {
    imap_dfr(topics, 
             ~data.frame(name = .y, topic = .x, package = cranscrub::ctv_pkgs(.y)))
    
  }
}

ctv_summary <- function(data) {
  
  db <- cranscrub::pkg_db(data$package) %>% 
    left_join(data, by = c("Package" = "package")) %>% 
    cranscrub::pkg_db_clean_depends() %>% 
    cranscrub::pkg_db_clean_imports() %>% 
    cranscrub::pkg_db_clean_suggests() %>% 
    cranscrub::pkg_db_clean_author()
  
  db %>% 
    mutate(nauthor = map_int(db$AuthorClean, ~length(.x[[1]]))) %>% 
    rowwise() %>% 
    mutate(intra_connect = any(c(DependsVec, ImportsVec, SuggestsVec) %in% data$package[data$name==name])) %>% 
    group_by(name, topic) %>% 
    summarise(n = n(), 
              ncontributors = length(unique(unlist(AuthorClean))), 
              avg_contributors = mean(nauthor),
              intra_perc = mean(intra_connect) * 100
              ) %>% 
    arrange(avg_contributors)
  
}

pkg_network <- function(db) {
  out <- db %>% 
    cranscrub::pkg_db_clean_depends() %>% 
    cranscrub::pkg_db_clean_imports() %>% 
    cranscrub::pkg_db_clean_suggests() %>% 
    select(Package, contains("Vec"))
  
  get_pkg_edges <- function(type) {
    out %>% 
      filter(!map_lgl({{type}}, ~if(length(.x)==1) identical(.x, list(character(0))) else FALSE)) %>% 
      select(from = {{type}}, to = Package) %>% 
      unnest_longer(from) %>% 
      filter(from %in% db$Package) %>% 
      mutate(Type = as_string(ensym(type)) %>% 
               str_replace("Vec", ""))
  }
  
  pkg_edges <- map_dfr(quos(DependsVec, ImportsVec, SuggestsVec), get_pkg_edges)
  graph_from_data_frame(pkg_edges,
                        vertices = unique(c(pkg_edges$to, pkg_edges$from)),
                        directed = TRUE) %>% 
    as_tbl_graph() 
    
}