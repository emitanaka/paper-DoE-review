## ---- include = FALSE---------------------------------------------------------
paper <- here::here("paper/index.Rmd")
meta <- rmarkdown::yaml_front_matter(paper)


## ----child=paper--------------------------------------------------------------

## ----setup, cache = FALSE, include = FALSE------------------------------------
library(tidyverse)
library(rvest)
library(lubridate)
library(cranlogs)
library(glue)
library(scales)
library(colorspace)
library(tidytext)
library(pluralize)
library(kableExtra)
library(igraph)
library(ggraph)
library(ggrepel)
library(patchwork)
library(ggwordcloud)
tocache <- TRUE
knitr::opts_chunk$set(echo = FALSE, 
                      cache = FALSE,
                      cache.path = "cache/",
                      fig.align = 'center', 
                      fig.pos = 'htbp', 
                      fig.path = "figures/",
                      fig.width = 6,
                      message = FALSE,
                      warning = FALSE)

theme_set(
  theme(panel.background = element_rect(fill = NA),
        panel.grid = element_line(color = "lightgray"),
        axis.text = element_text(color = "black"),
        axis.line = element_line(color = "black", size = 0.7),
        axis.ticks.length = unit(1.4, "mm"),
        axis.ticks = element_line(color = "black", size = 0.7),
        axis.title = element_text(color = "black", face = "bold"),
        strip.background = element_rect(color = "black",
                                        fill = "black"),
        strip.text = element_text(color = "white"),
        plot.title.position = "plot",
        plot.title = element_text(color = "black", hjust = 0)))

ctv <- function(view) {
  ifelse(knitr::is_html_output(), glue("[{view}](https://cran.r-project.org/web/views/{view}.html)"), glue("\\ctv{[view]}", .open = "[", .close = "]"))
}
CRANpkg <- function(pkg) {
  ifelse(knitr::is_html_output(), glue("[{pkg}](https://cran.r-project.org/web/packages/{pkg}/index.html)"), glue("\\CRANpkg{[pkg]}", .open = "[", .close = "]"))
}


## ----ctv----------------------------------------------------------------------
doe_pkgs <- ctv:::.get_pkgs_from_ctv_or_repos("ExperimentalDesign", 
                                              repos = "http://cran.rstudio.com/")[[1]]
doe_pkgs <- c(doe_pkgs, "DeclareDesign")
npkgs <- length(doe_pkgs)


## ----cran-data, cache = tocache-----------------------------------------------
end <- Sys.Date() - 2 # usually 1-2 days data are not available yet
start <- end - years(2) + days(2)
dldat <- cran_downloads(doe_pkgs, from = start, to = end)


## ----cran-data2, cache = tocache----------------------------------------------
start2 <- end - years(5) + days(2)
dldat2 <- cran_downloads(doe_pkgs, from = start2, to = end)


## ----download-dist, dependson = "cran-data"-----------------------------------
dldat_sum <- dldat %>% 
  group_by(package) %>% 
  summarise(total = sum(count)) %>% 
  arrange(desc(total)) %>% 
  mutate(rank = 1:n())

dl1plot <- ggplot(dldat_sum, aes(x = "", y = total/1000)) + 
  geom_violin() +
  geom_boxplot(width = 0.1) + 
  scale_y_log10(label = comma) + 
  labs(x = "",
       y = "Download counts in the last 2 year ('000s)", title = "(A)") + 
  geom_text_repel(data = slice_max(dldat_sum, total, n = 13), 
                  nudge_x = rep(c(0.2, -0.2, 0.2, -0.2, 0.15), c(3, 2, 2, 3, 3)),
                  size = 4,
                  aes(label = package)) + 
  theme(axis.ticks.length.x = unit(0, "mm"))


dldat_sum5 <- dldat2 %>% 
  group_by(package) %>% 
  summarise(total = sum(count)) %>% 
  arrange(desc(total)) %>% 
  mutate(rank = 1:n())

top15 <- dldat_sum %>% 
  left_join(dldat_sum5, by = "package") %>% 
  slice_max(total.x + total.y, n = 15) %>% 
  select(package, rank.x, rank.y) %>% 
  pivot_longer(-package) %>% 
  mutate(name = ifelse(name=="rank.x", "Last 2 years", "Last 5 years"))

dl2plot <- ggplot(top15, aes(name, value, group = package)) +
  geom_line() + 
  geom_point() +
  scale_y_reverse(breaks = 1:15) +
  scale_x_discrete(expand = expansion(c(0.1, 0.5))) +
  labs(x = "", y = "Ranking", title = "(B)") +
  geom_text(data = filter(top15, name=="Last 5 years"),
            aes(label = package), 
            hjust = 0, 
            nudge_x = 0.1,
            size = 4) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


## ----dlplots, fig.cap=glue("The above graph shows the total download counts from {start} to {end} of DoE packages."), fig.width = 6----
dl1plot + dl2plot


## ----text, cache=tocache------------------------------------------------------
url <- "http://cran.rstudio.com/web/packages/packages.rds"
db <- readRDS(url(url)) %>% 
  as.data.frame()

doe_db <- db %>% 
  filter(Package %in% doe_pkgs) %>% 
  mutate(Description = str_replace_all(Description, "\n", " "),
         Description = str_squish(Description),
         Title = str_replace_all(Title, "\n", " "))


## ----fns----------------------------------------------------------------------
singularize2 <- function(x) {
    res <- singularize(x)
    res <- ifelse(res=="bia", "bias", res)
    ifelse(res=="baye", "bayes", res)
}
stop_words_rex <- c(paste0("^", stop_words$word, "$"), "^doi", "^[0-9.]+$")
get_ngram <- function(ngram, desc = c("Title", "Description")) {
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


## ----ngrams, cache=tocache, dependson="fns"-----------------------------------
bigram_title <- get_ngram(2, "Title")
bigram_desc <- get_ngram(2, "Description")
trigram_desc <- get_ngram(3, "Description")


## ----wordcloud, fig.height = 3.3----------------------------------------------
dldat_comb_bigram_title <- combine_dl(bigram_title, Title)
dldat_comb_bigram_desc <- combine_dl(bigram_desc, Title)
dldat_comb_trigram_desc <- combine_dl(trigram_desc, Title)

set.seed(1)
ggplot(rbind(dldat_comb_bigram_desc,
             dldat_comb_trigram_desc),
       aes(label = word, size = total, color = n)) +
  geom_text_wordcloud(shape = "square",
                      show.legend = TRUE) +
  scale_size_area(max_size = 8) +
  guides(size = FALSE) +
  scale_color_continuous(breaks = 1:11) +
  theme(legend.position = "bottom")


## ----cran-titles--------------------------------------------------------------
url <- "http://cran.rstudio.com/web/packages/packages.rds"
db <- readRDS(url(url)) %>% 
  as.data.frame()


## ----bigram-------------------------------------------------------------------
stop_words_ext <- c(stop_words$word, "doi")

doe_db <- db %>% 
  filter(Package %in% doe_pkgs) %>% 
  mutate(Description = str_replace_all(Description, "\n", " "),
         Description = str_squish(Description),
         Title = str_replace_all(Title, "\n", " "))

bigram_tab <- function(data, col) {
  data %>% 
    unnest_tokens(word, {{col}}, token = "ngrams", n = 2) %>% 
    separate(word, c("word1", "word2"), sep = " ") %>% 
    mutate(word1 = singularize(word1),
           word2 = singularize(word2)) %>% 
    # don't count the same bigram within the same package
    distinct(Package, word1, word2) %>% 
    filter(!word1 %in% stop_words_ext,
           !word2 %in% stop_words_ext,
           !str_detect(word1, "^[0-9.]+$"),
           !str_detect(word2, "^[0-9.]+$")) %>% 
    count(word1, word2, sort = TRUE)  
}


## ----bigram-title, results="asis"---------------------------------------------
btitle <- bigram_tab(doe_db, Title) %>% 
  filter(n > 3) %>% 
  mutate(word = paste(word1, word2)) %>% 
  select(word, n) %>% 
  kbl(booktabs = TRUE, linesep = "")

bdesc <- bigram_tab(doe_db, Description) %>% 
  filter(n > 4) %>% 
  mutate(word = paste(word1, word2)) %>% 
  select(word, n) %>% 
  kbl(booktabs = TRUE, linesep = "")

cat(c("\\begin{table}[h] \\centering ", 
      btitle,
    "\\hspace{1cm} \\centering ",
      bdesc,
    "\\caption{My tables} \\end{table}")) 


## -----------------------------------------------------------------------------
doe_imports <- doe_db %>% 
  mutate(Depends = str_replace_all(Depends, "\n", " "),
         Depends = str_replace_all(Depends, fixed("("), " ("),
         Imports = str_replace_all(Imports, "\n", " "),
         Imports = str_replace_all(Imports, fixed("("), " ("),
         imports = str_c(Depends, Imports, sep = ","),
         imports = str_split(imports, ","),
         imports = map(imports, ~{
           str_squish(.x) %>% 
             word() %>% 
             .[.!=""]}
         ),
         imports_doe = map(imports, ~.x[.x %in% doe_pkgs])) %>% 
  select(Package, imports_doe) %>% 
  unnest_longer(imports_doe) %>% 
  filter(!is.na(imports_doe)) %>% 
  rename(from = imports_doe, to = Package) %>% 
  select(from, to)


## ----doe-network, eval = FALSE------------------------------------------------
#> graph_from_data_frame(doe_imports) %>%
#>   ggraph(layout = 'fr') +
#>   geom_edge_link(aes(start_cap = label_rect(node1.name),
#>                      end_cap = label_rect(node2.name)),
#>                  arrow = arrow(length = unit(2, 'mm')),
#>                  color = "#79003e") +
#>   geom_node_text(aes(label = name),
#>                  color = "#79003e") +
#>   theme(panel.background = element_rect(fill = "#f6e5ee",
#>                                         color = "#79003e"),
#>         plot.margin = margin(20, 20, 20, 20))


