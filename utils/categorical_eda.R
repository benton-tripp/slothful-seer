# EDA for categorical data
cat.eda <- function(df, .type=c("bar", "pie", "table")) {
  .type <- match.arg(.type)
  if (.type == "bar") {
    # Bar plot for biome variable
    out <- ggplot(df %>% arrange(biome), 
                  aes(x=factor(biome, levels=seq(1,14)))) + 
      geom_bar(fill='steelblue', alpha=0.7) +
      labs(x='Biome', y='Count', title='Bar Plot of Biomes') +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      theme(text = element_text(size = 16), 
            axis.text.y = element_text(size = 16),
            axis.text.x = element_text(size = 16),
            axis.title = element_text(size = 17), 
            plot.title = element_text(size = 18))
  } 
  
  if (.type == "pie") {
    # Pie chart for biome variable
    out <- df %>% 
      count(biome) %>%
      arrange(as.integer(biome)) %>%
      ggplot(aes(x = "", y = n, fill = factor(biome))) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      theme_void() +
      labs(fill = "Biome", title = "Pie Chart of Biome")  + 
      theme(text = element_text(size = 16), 
            axis.text.y = element_text(size = 16),
            axis.text.x = element_text(size = 16),
            axis.title = element_text(size = 17), 
            plot.title = element_text(size = 18))
  } 
  
  if (.type == "table") {
    # Count table for biome variable
    out <- df %>% count(biome) %>% mutate(biome=as.integer(biome))
  }
  out
}