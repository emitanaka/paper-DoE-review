
save_output <- function(obj) {
  # file name derived from object
  fn <- rlang::as_string(rlang::enexpr(obj))
  saveRDS(obj, here::here(glue::glue("output/{fn}.rds")))
}

ctv_pkgs <- function(ctv_table, ctv_name) {
  ctv_table %>% 
    filter(name == ctv_name) %>% 
    pull(packages) %>% 
    .[[1]]
}

ctv_pkg_db <- function(ctv_table) {
  unlist(ctv_table$packages) %>% 
    unique() %>% 
    cranscrub::pkg_db() %>% 
    cranscrub::pkg_db_clean_depends() %>% 
    cranscrub::pkg_db_clean_imports() %>% 
    cranscrub::pkg_db_clean_suggests() %>% 
    cranscrub::pkg_db_clean_author()
}


pkg_update_dates <- function(pkgs, message = TRUE) {
  res <- map(pkgs, function(pkg) {
    if(message) message(paste("Package", pkg))
    pkgsearch::cran_package_history(pkg) %>% 
      select(package = Package, update = date) %>% 
      mutate(update = as.Date(update))
  })
  do.call("rbind", res)
}

pkg_cran_downloads <- function(pkgs, start, end) {
  # it can have trouble downloading long periods so break it up by year for 
  # a package at a time
  map_dfr(pkgs, function(apkg) {
    map_dfr(start:end, ~{
      start_date <- as.Date(paste0(.x, "-01-01"))
      end_date <- as.Date(paste0(.x, "-12-31"))
      cran_downloads(apkg, from = start_date, to = end_date)  
    })
  })
}


ctv_combine_pkg_data <- function(ctv_table,
                                 pkg_downloads,
                                 pkg_db_clean,
                                 pkg_updates) {
  nest_pkg_downloads <- nest(pkg_downloads, downloads = -package)
  nest_pkg_updates <- nest(pkg_updates, updates = -package)
  select_pkg_db <- select(pkg_db_clean, 
                          package = Package, Title, Description, AuthorClean,
                          DependsVec, ImportsVec, SuggestsVec)
  ctv_table %>% 
    unnest(packages) %>% 
    rename(package = packages) %>% 
    left_join(nest_pkg_downloads, by = "package") %>% 
    left_join(nest_pkg_updates, by = "package") %>% 
    left_join(select_pkg_db, by = "package") %>% 
    mutate(first = map_dbl(updates, ~min(.x$update)),
           first = as.Date(first, origin = "1970-01-01"))
    
}


ctv_downloads_rank_by_year <- function(ctv_pkg_data) {
  ctv_pkg_data %>% 
    unnest_longer(downloads) %>% 
    mutate(year = year(downloads$date)) %>% 
    group_by(name, topic, package, year, first) %>% 
    summarise(total = sum(downloads$count),
              nupdates = map2_dbl(updates, year, ~{
                if(is.null(.x$update)) return(0)
                sum(year(.x$update) <= .y)
              })) %>% 
    ungroup() %>% 
    distinct() %>% 
    filter(year > year(first),
           year != year(Sys.Date())) %>% 
    arrange(desc(total), year) %>% 
    group_by(year, name) %>% 
    mutate(rank = 1:n()) %>% 
    ungroup() 
}

ctv_gini_by_year <- function(ctv_rank_yearly) {
  ctv_rank_yearly %>% 
    group_by(name, year) %>% 
    summarise(gini = ineq::Gini(total),
              n = n()) %>% 
    ungroup()
}

ctv_lorenz_by_year <- function(ctv_rank_yearly) {
  ctv_rank_yearly %>% 
    group_by(name, year) %>% 
    summarise(lorenz_data = data.frame(p = ineq::Lc(total)$p, 
                                       L = ineq::Lc(total)$L)) %>% 
    ungroup()
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
    distinct(!!!map(c("package", words), as.name)) %>% 
    # allow stop words in between - not relant for bigrams
    filter(if_all(num_range("word", c(1, ngram)), 
                  ~!str_detect(., paste0(stop_words_rex, collapse = "|")))) %>% 
    mutate(word = do.call("paste", map(words, ~eval(parse(text = .x))))) %>% 
    filter(!str_detect(word, DUMMYWORD)) %>% 
    group_by(word) %>% 
    summarise(n = n(), pkgs = list(package)) %>% 
    arrange(desc(n)) %>% 
    mutate(ngram = ngram)
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





ctv_summary <- function(ctv_pkg_data) {
  ctv_pkg_data %>% 
    mutate(nauthor = map_int(AuthorClean, ~length(.x[[1]]))) %>% 
    rowwise() %>% 
    mutate(intra_connect = any(c(DependsVec, ImportsVec, SuggestsVec) %in% ctv_pkg_data$package[ctv_pkg_data$name==name])) %>% 
    group_by(name, topic) %>% 
    summarise(n = n(), 
              ncontributors = length(unique(unlist(AuthorClean))), 
              avg_contributors = mean(nauthor),
              intra_perc = mean(intra_connect) * 100
              ) %>% 
    arrange(avg_contributors)
  
}

pkg_network <- function(data) {
  out <- data %>% 
    select(package, contains("Vec"))
  
  get_pkg_edges <- function(type) {
    out %>% 
      filter(!map_lgl({{type}}, ~if(length(.x)==1) identical(.x, list(character(0))) else FALSE)) %>% 
      select(from = {{type}}, to = package) %>% 
      unnest_longer(from) %>% 
      filter(from %in% data$package) %>% 
      mutate(Type = as_string(ensym(type)) %>% 
               str_replace("Vec", ""))
  }
  
  pkg_edges <- map_dfr(quos(DependsVec, ImportsVec, SuggestsVec), get_pkg_edges)
  graph_from_data_frame(pkg_edges,
                        vertices = unique(c(pkg_edges$to, pkg_edges$from)),
                        directed = TRUE) %>% 
    as_tbl_graph() 
    
}