
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
    scale_y_log10(label = comma, expand = c(0.1, 0)) +
    geom_label(data = mutate(ginidata, year = as.factor(year)), 
               size = 3,
               aes(y = 900, label = paste0(scales::percent(gini, 0.1),
                                           "\nn=", n)))
}

plot_lorenz_curve <- function(data, year) {
  
  dat <- data %>% 
    filter(year == .env$year)
  
  data <- data.frame(p = ineq::Lc(dat$total)$p, L = ineq::Lc(dat$total)$L) 
  anndata <- data %>% 
    arrange(desc(L)) %>% 
    filter(L < 0.5) %>% 
    slice(1)
  ggplot(data, aes(p * 100, L * 100)) +
    geom_abline(color = "red") +
    geom_line() + 
    geom_point() +
    geom_ribbon(aes(ymin = 0, ymax = L * 100),
                fill = "#009E73", alpha = 0.5) +
    geom_ribbon(aes(ymin = L * 100, ymax = p * 100),
                 fill = "#D55E00", alpha = 0.5) +
    annotate("text", x = 75, y = 80, label = "Line of perfect equality",
             angle = 45)# +
    # geom_segment(data = anndata,
    #              aes(x = p * 100, xend = p * 100,
    #                  y = 0, yend = L * 100),
    #              linetype = "dashed") +
    # geom_segment(data = anndata,
    #              aes(x = 0, xend = p * 100,
    #                  y = L * 100, yend = L * 100),
    #              linetype = "dashed") +
    # scale_x_continuous(breaks = c(0, 25, 50, 75, round(anndata$p * 100), 100), 
    #                    expand = c(0, 0)) +
    # scale_y_continuous(breaks = c(0, 25, round(anndata$L * 100), 50, 75, 100),
    #                    expand = c(0, 0))
  
}

plot_download_trend <- function(data, updates) {
  top_pkgs <- unique(updates$pkg)
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
    geom_vline(data = rename(updates, package = pkg), 
               aes(xintercept = update),
               color = "red", linetype = "dashed") +
    geom_line() +
    scale_y_log10() +
    scale_x_date(expand = c(0, 0)) + 
    facet_grid(package ~ .) +
    theme(legend.position = "bottom")
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
                                  "#88CCEE", "#CC6677", "#DDCC77", 
                                  "#117733", "#DDAA33", "#BB5566"),
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


plot_word_cloud_over_years <- function(data) {
  data %>% 
    filter(perc >= 10) %>% 
    ggplot(aes(label = word, size = perc, color = n)) +
    geom_text_wordcloud(shape = "square",
                        show.legend = TRUE,
                        rm_outside = FALSE) +
    scale_size_area(max_size = 4.5) +
    guides(size = "none") +
    colorspace::scale_color_continuous_sequential(breaks = round(seq(2, 13, length.out = 4)),
                                                  palette = "Blues") +
    theme(legend.position = "bottom",
          text = element_text(size = 18)) +
    labs(color = "# of packages") +
    guides(color = guide_colorbar(title.position = "top")) + 
    facet_grid(year ~ .) 
}

plot_word_cloud <- function(data) {
  data %>% 
    filter(perc >= 1) %>% 
    ggplot(aes(label = word, size = perc, color = n)) +
    geom_text_wordcloud(shape = "square",
                        show.legend = TRUE,
                        rm_outside = TRUE) +
    scale_size_area(max_size = 18) +
    guides(size = "none") +
    colorspace::scale_color_continuous_sequential(breaks = round(seq(2, 13, length.out = 4)),
                                                  palette = "Blues") +
    theme(legend.position = "bottom",
          text = element_text(size = 18)) +
    labs(color = "# of packages") +
    guides(color = guide_colorbar(title.position = "top")) 
}

plot_pkg_network <- function(data) {
  data %>% 
    ggraph('fr') +
    geom_edge_link(aes(color = Type),
                   arrow = arrow(length = unit(3, 'mm')), 
                   end_cap = circle(3, 'mm')) + 
    geom_node_point(size = 4) +
    geom_node_text(aes(label = name), repel = TRUE, size = 3.5) +
    theme_graph(base_family = "Helvetica") + 
    labs(color = "")
  
}

plot_pkg_network_by_ctv <- function(name) {
  cranscrub::ctv_pkgs(name) %>% 
    cranscrub::pkg_db() %>% 
    pkg_network() %>% 
    plot_pkg_network()
}