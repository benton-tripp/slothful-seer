
plot.preds <- function(pred.lis, plot.type, model.names) {
  plots <- purrr::map(seq_along(pred.lis), function(i) {
    model.name <- model.names[i]
    pred.data <- pred.lis[[i]]
    test.df <- pred.lis[[i]]$test %>%
      mutate(outcome = factor(case_when(yhat == y & yhat == "presence" ~ "TP",
                                 yhat == y & yhat == "no.presence" ~ "TN",
                                 yhat != y & yhat == "presence" ~ "FP",
                                 yhat != y & yhat == "no.presence" ~ "FN"),
                              levels=c("FP", "FN", "TN", "TP")))
    
    if (plot.type == "Estimated Probability Raster") {
      # saveRDS(pred.data, "tests/pred.data.rds")
      # saveRDS(model.name, "tests/model.name.rds")
      # saveRDS(test.df, "tests/test.df.rds")
      
      raster.df <- as.data.frame(rasterToPoints(pred.data$raster))
      plt <- ggplot()  +
        geom_tile(data = raster.df, aes(x = x, y = y), width = 1, height = 1, color="black", linewidth=1.1) +
        geom_tile(data = raster.df, aes(x = x, y = y, fill = layer), width = 1, height = 1) +
        scale_fill_gradientn(colors = rev(terrain.colors(10))) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        theme_bw() + 
        theme(text= element_text(size=11),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 14, face = "bold")) +
        labs(x = "Longitude", y = "Latitude", title=paste(model.name, 
                                                          "Estimated Probability")) +
        coord_sf(xlim = c(min(raster.df$x, na.rm=T), max(raster.df$x, na.rm=T)))
      
    } else if (plot.type == "Predicted vs. Actual Map") {
      
      plt <- ggplot(data = test.df, aes(x=lon, y=lat, color = outcome, shape = outcome)) +
        geom_point(size = 2) +
        theme_minimal() + 
        scale_color_manual(values = c("TP" = "blue", "TN" = "blue",
                                      "FP" = "red", "FN" = "red")) +
        scale_shape_manual(values = c("TP" = 21, "TN" = 4, "FP" = 21, "FN" = 4)) +
        theme(legend.key = element_rect(color = "white"),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 14, face = "bold")) +
          labs(x = "Longitude", y = "Latitude", 
               title=paste(model.name, "Actual vs. Predicted")) +
          coord_equal() 
      
    } else if (plot.type == "Probability Density Plot") {
      
      if (class(pred.data$probs) == "data.frame") {
        probs <- pred.data$probs$presence 
      } else {
        probs <- pred.data$probs
      }
      plt <- ggplot(data.frame(probs = pred.data$probs), aes(probs)) + 
        geom_density(fill = "blue", alpha = 0.5, color="black") + 
        theme_minimal() + 
        ggtitle(paste0("Distribution of ", model.name, " Pred. Probability"))
      
    } else if (plot.type == "Bar Plot") {
      plt.data <- tidyr::pivot_longer(test.df, c("y", "yhat")) %>%
        mutate(`Predicted/Actual` = factor(ifelse(name == "y", "Actual", "Predicted"), 
                             levels=c("Actual", "Predicted")))
      actual.total <- plt.data %>% 
        filter(`Predicted/Actual` == "Actual") %>% 
        summarize(n=n()) %>% ungroup() %>% pull()
      plt.data <- plt.data %>%
        group_by(value, `Predicted/Actual`) %>%
        summarize(`%` = round(n()/actual.total * 100, 2), .groups="keep")
      plt <- ggplot(plt.data, aes(x=`Predicted/Actual`, y=`%`, fill=value)) + 
        geom_bar(stat="identity", color="black", alpha = 0.8, position="dodge") +
        theme_minimal() + 
        labs(title=paste(model.name, "Predicted Presence % vs. Actuals"))
    } 
    plt + theme(axis.text.x = element_text(size = 12),
                text = element_text(size = 12), 
                axis.text.y = element_text(size = 12),
                axis.title = element_text(size = 12.5), 
                plot.title = element_text(size = 14))
  })
  # Combine plots
  plots <- ggpubr::ggarrange(plotlist = plots, ncol = 3, nrow = 2)
  return(plots)
}

# Define a helper function to apply the color styling
apply.color.style <- function(dt, cm.df, value.col, color.col) {
  unique.values <- unique(cm.df[[value.col]])
  color.values <- cm.df[[color.col]][match(unique.values, cm.df[[value.col]])]
  dt %>%
    formatStyle(columns = value.col, 
                backgroundColor = styleEqual(unique.values, color.values))
}


show.cms <- function(pred.lis, model.names) {
  cms <- purrr::map(seq_along(pred.lis), function(i) {
    model.name <- model.names[i]
    pred.data <- pred.lis[[i]]
    cm <- pred.data$cm$table %>% as_tibble() %>% rename(Freq = "n")
    
    # Calculate percentages
    total.positive <- sum(filter(cm, Reference == "presence")$Freq)
    total.negative <- sum(filter(cm, Reference == "no.presence")$Freq)
    
    tp <- cm$Freq[cm$Prediction == "presence" & cm$Reference == "presence"] / total.positive
    tn <- cm$Freq[cm$Prediction == "no.presence" & cm$Reference == "no.presence"] / total.negative
    fp <- cm$Freq[cm$Prediction == "presence" & cm$Reference == "no.presence"] / total.negative
    fn <- cm$Freq[cm$Prediction == "no.presence" & cm$Reference == "presence"] / total.positive
    
    # Convert percentages to color intensities
    color.map <- data.frame(
      Prediction = c("presence", "no.presence", "presence", "no.presence"),
      Reference = c("presence", "no.presence", "no.presence", "presence"),
      Color = c(rgb(0, 1, 0, tp), rgb(0, 1, 0, tn), rgb(1, 0, 0, fp), rgb(1, 0, 0, fn))
    )
    
    cm <- left_join(cm, color.map, by = c("Prediction", "Reference")) %>%
      mutate(Label = factor(case_when(Prediction == Reference & Prediction == "presence" ~ "TP",
                                        Prediction == Reference & Prediction == "no.presence" ~ "TN",
                                        Prediction != Reference & Prediction == "presence" ~ "FP",
                                        Prediction != Reference & Prediction == "no.presence" ~ "FN"),
                              levels=c("FP", "FN", "TN", "TP"), 
                              labels=c("False Positive", "False Negative", 
                                       "True Negative", "True Positive")))
    
    cm
  })
  
  names(cms) <- model.names
  
  cm.df <- do.call(rbind, purrr::map(model.names, ~mutate(cms[[.x]], Model = .x))) %>%
    tidyr::pivot_wider(names_from="Model", 
                       values_from=c("Freq", "Color"),
                       names_sep=".") 
  names(cm.df) <- names(cm.df) %>% gsub("Freq.", "", .)
  
  dt <- datatable(cm.df[, 1:8], 
            rownames=F,
            selection='none',
            options=list(
              paging=F,
              scrollX=F,
              scrollY=F,
              searching=F,
              orderMulti=F,
              info=F,
              lengthChange=F,
              pageLength=4
            )
  ) %>%
    formatStyle(columns=1:8, `font-size`="15.5px")
  
  # Apply the color styling for each model column
  color.cols <- paste0("Color.", model.names)
  
  for (i in seq_along(model.names)) {
    dt <- apply.color.style(dt, cm.df, model.names[i], color.cols[i])
  }
  
  dt
}
