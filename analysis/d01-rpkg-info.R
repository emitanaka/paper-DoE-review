library(cranlogs)
library(tidyverse)
library(yaml)
library(here)
url <- "http://cran.rstudio.com/web/packages/packages.rds"
db <- readRDS(url(url)) %>% 
  as.data.frame()%>% 
  mutate(Description = str_replace_all(Description, "\n", " "),
         Description = str_squish(Description),
         Title = str_replace_all(Title, "\n", " "))

doe_pkgs <- ctv:::.get_pkgs_from_ctv_or_repos("ExperimentalDesign", 
                                              repos = "http://cran.rstudio.com/")[[1]]
dl <- cran_downloads(packages = doe_pkgs,
                     from = "2020-01-01",
                     to = "2020-12-31")
dlsum <- dl %>% 
  group_by(package) %>% 
  summarise(total2020 = sum(count)) %>% 
  arrange(desc(total2020)) %>% 
  left_join(select(db, 
                   package = Package, 
                   depends = Depends,
                   imports = Imports,
                   suggest = Suggests,
                   author = Author,
                   date = Date,
                   version = Version,
                   title = Title,
                   description = Description),
            by = "package") %>% 
  mutate(author = str_replace_all(author, "(\\(.+\\)|\\n)", ""),
         author = str_squish(author))

out <- pmap(dlsum, function(package, total2020, depends, imports, suggests, author, date, version, title, description) {
  x <- list(list(download2020 = as.integer(total2020),
                 depends = ifelse(is.na(depends), "", depends), 
                 imports = ifelse(is.na(imports), "", imports), 
                 suggests = ifelse(is.na(suggests), "", suggests), 
                 author = ifelse(is.na(author), "", author), 
                 date = ifelse(is.na(date), "", date), 
                 version = ifelse(is.na(version), "", version), 
                 title = ifelse(is.na(title), "", title), 
                 description = ifelse(is.na(description), "", description),
                 type = '',
                 main = ''))
  names(x) <- package
  x
})
saveRDS(out, file = here("data/derived/doe-pkgs.rds"))
out <- readRDS(here::here("data/derived/doe-pkgs.rds"))
yaml::write_yaml(out, here::here("data/input/doe-pkgs-original.yml"))
