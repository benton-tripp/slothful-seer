
presence.eda <- function(df, .type=c("Bar", "Pie", "Table")) {
  .type <- match.arg(.type)
  if (.type == "Bar") {
    # Bar plot for presence variable
    out <- ggplot(df, aes(x=factor(presence))) + 
      geom_bar(fill='steelblue', alpha=0.7) +
      labs(x='Presence', y='Count', title='Presence vs. (Pseudo) Absence') +
      theme_minimal()  + 
      theme(text = element_text(size = 16), 
            axis.text.y = element_text(size = 16),
            axis.text.x = element_text(size = 16),
            axis.title = element_text(size = 17), 
            plot.title = element_text(size = 18))
  } else if (.type == "Pie") {
    # Pie chart for presence variable
    out <- df %>% 
      count(presence) %>%
      ggplot(aes(x = "", y = n, fill = factor(presence))) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      theme_void() +
      labs(fill = "Presence", title = "Presence vs. (Pseudo) Absence")   + 
      theme(text = element_text(size = 16), 
            plot.title = element_text(size = 18))
  } else if (.type == "Table") {
    out <- # Count table for presence variable
      df %>% 
      count(presence) %>%
      datatable(
        filter='none',
        selection='none',
        rownames=F,
        options=list(
          paging=F,
          searching=F,
          orderMulti=T,
          info=F,
          lengthChange = F,
          pageLength=10
        )
      ) %>%
      formatStyle(columns=names(dt), `font-size`="20px")
  }
  out
}