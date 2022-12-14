# main HTML
ctv <- function(view) glue("[{view}](https://cran.r-project.org/web/views/{view}.html)")
CRANpkg <- function(pkg) glue("[`{pkg}`](https://cran.r-project.org/web/packages/{pkg}/index.html)")
ref <- function(x) glue("\\@ref({x})")
secref <- function(x, name) glue("Section \\@ref({x})")
url <- function(x) glue("[{x}]({x})")