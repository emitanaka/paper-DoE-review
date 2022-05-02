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

knitr::opts_chunk$set(echo = FALSE, 
                      cache = TRUE,
                      cache.path = "cache/")

myggtheme <- 
  theme(panel.background = element_rect(fill = NA),
        panel.grid = element_line(color = "#f6e5ee"),
        axis.text = element_text(color = "#79003e"),
        axis.line = element_line(color = "#79003e", size = 0.7),
        axis.ticks.length = unit(1.4, "mm"),
        axis.ticks = element_line(color = "#79003e", size = 0.7),
        axis.title = element_text(color = "#79003e", face = "bold"),
        strip.background = element_rect(color = "#79003e",
                                        fill = "#AD0059"),
        strip.text = element_text(color = "white"),
        plot.title.position = "plot",
        plot.title = element_text(color = "#79003e", face = "bold")) 


## ----cran-titles--------------------------------------------------------------
# Thanks to Dirk Eddelbuettel's answer on SO:
# https://stackoverflow.com/questions/11560865/list-and-description-of-all-packages-in-cran-from-within-r
url <- "http://cran.rstudio.com/web/packages/packages.rds"
db <- readRDS(url(url)) %>% 
  as.data.frame()


## -----------------------------------------------------------------------------
nanalysis <- db %>% 
  filter(str_detect(tolower(Title), "analysis")) %>% 
  nrow()

ndesign <- db %>% 
  filter(str_detect(tolower(Title), "design")) %>% 
  nrow()


## ----doe-cran-----------------------------------------------------------------
dat_DoE <- read_html("https://cran.r-project.org/web/views/ExperimentalDesign.html")
date_download <- Sys.Date()
cran_names <- available.packages(contriburl = "http://cran.rstudio.com/") %>% 
  rownames() %>% 
  unique() # it should be unique
doe_pkgs <- dat_DoE %>% 
  html_nodes("li") %>% 
  html_nodes("a") %>% 
  html_text() %>% 
  .[. %in% cran_names] %>% 
  unique()

dat_survey <- read_html("https://cran.r-project.org/web/views/OfficialStatistics.html")
survey_pkgs <- dat_survey %>% 
  html_nodes("li") %>% 
  html_nodes("a") %>% 
  html_text() %>% 
  .[. %in% cran_names] %>% 
  unique()


## ---- Rlogo, echo=FALSE, fig.cap='The logo of R.', out.width='2in', fig.align='center', fig.pos='htbp'----
knitr::include_graphics(here::here('Rlogo.pdf'))

