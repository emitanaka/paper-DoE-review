# arxiv
ctv <- function(view) glue("\\ctv{[view]}", .open = "[", .close = "]")
CRANpkg <- function(pkg) glue("\\CRANpkg{[pkg]}", .open = "[", .close = "]")
ref <- function(x) glue("\\ref{[x]}", .open = "[", .close = "]")
secref <- function(x, name) glue("Section \\ref{[x]}", .open = "[", .close = "]")
url <- function(x) glue("\\url{[x]}", .open = "[", .close = "]")
