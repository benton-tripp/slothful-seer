cont.plt <- function(df, .var, .type) {
  p <- ggplot(df, aes(x = !!sym(.var)))
  if (.type == "Histogram") { 
    p <- p + geom_histogram(bins = 30, fill = '#f78f88', alpha = 0.5, color="black") 
  } else if (.type == "Density") {
    p <- p + geom_density(color="black", fill='#f78f88', alpha=0.5, linewidth=1) 
  } else if (.type == "Combined Plot") {
    p <- p + geom_histogram(aes(y=after_stat(density)), 
                            bins = 30, fill = '#f78f88', alpha = 0.5) +
      geom_density(color="black", linewidth=1) 
  } 
  p <- p + theme_minimal() +
    labs(title = paste("Distribution of ", .var),
         x = .var,
         y = ifelse(.type %in% c("Density", "Combined Plot"), "Density", "Frequency"))
}

cont.eda <- function(df, .var="All", .type=c("Density", "Histogram", "Combined Plot", "Table")) {
  .type <- match.arg(.type)
  
  if (.type == "Table") {
    # Filter out only the required variables
    out <- df %>% 
      dplyr::select(-c("presence", "biome")) %>%
      skim_without_charts() %>%
      dplyr::select(-skim_type, -complete_rate) %>%
      rename(variable = skim_variable) 
  } else {
    if (.var == "All") {
      out <- map(names(df)[!(names(df) %in% c("presence", "biome"))], 
                 ~cont.plt(df, .x, .type) + theme(plot.title = element_text(size = 11))) %>%
        ggarrange(plotlist=., ncol=4, nrow=3) 
    } else {
      out <- cont.plt(df, .var, .type)
    }
  }
  out
}