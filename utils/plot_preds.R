
plot.preds <- function(pred.lis, plot.type, model.names) {
  browser()
  plots <- purrr::map(seq_along(pred.lis), function(i) {
    model.name <- model.names[i]
    pred.data <- pred.lis[[i]]
    test.df <- pred.lis[[i]]$test %>%
      mutate(outcome = factor(case_when(yhat == y & yhat == "presence" ~ "TP",
                                 yhat == y & yhat == "no.presence" ~ "TN",
                                 yhat != y & yhat == "presence" ~ "FP",
                                 yhat != y & yhat == "no.presence" ~ "FN"),
                              levels=rev(c("FP", "FN", "TN", "TP")))) %>%
      as_tibble()
    if (plot.type == "Raster Images") {
      raster.df <- as.data.frame(rasterToPoints(pred.data$raster))
      plt <- ggplot() +
        geom_tile(data = raster.df, aes(x = x, y = y, fill = layer), width = 1, height = 1) +
        scale_fill_gradientn(colors = rev(terrain.colors(10))) +
        theme_minimal() + 
        theme(axis.text = element_text(size = 12),
              axis.title = element_text(size = 14, face = "bold")) +
        labs(x = "Longitude", y = "Latitude", title=paste(model.name, "Estimated Probability")) +
        coord_equal() 
    } else if (plot.type == "Test Data Outcomes") {
      plt <- ggplot(data = test.df, aes(x=lon, y=lat, color = outcome, shape = outcome)) +
        geom_point(size = 3) +
        theme_minimal() + 
        scale_color_manual(values = c("TP" = "blue", "TN" = "blue",
                                      "FP" = "red", "FN" = "red")) +
        scale_shape_manual(values = c("TP" = 21, "TN" = 4, "FP" = 4, "FN" = 21)) +
        theme(legend.key = element_rect(color = "white"),
              axis.text = element_text(size = 12),
              axis.title = element_text(size = 14, face = "bold")) +
          labs(x = "Longitude", y = "Latitude", 
               title=paste(model.name, "Actual vs. Predicted")) +
          coord_equal() 
    } else if (plot.type == "Probability Density Plots") {
      plt <- ggplot(data.frame(probs = pred.data$probs), aes(probs)) + 
        geom_density(fill = "blue", alpha = 0.5, color="black") + 
        theme_minimal() + 
        ggtitle(paste0(model.name, " Predicted Probability"))
    } else if (plot.type == "Bar Plots") {
      plt <- ggplot(data.frame(prediction=pred.data$yhat), aes(prediction)) + 
        geom_bar(fill = "blue", color="black", alpha = 0.8) + 
        geom_bar(data=data.frame(actual=pred.data$y), aes(actual), fill = "darkred", 
                 color="black", alpha = 0.8, position="dodge")
        theme_minimal() + 
        ggtitle(paste(model.name, "Prediction Totals vs. Actuals"))
    } 
    plt + theme(axis.text.x = element_text(size = 12),
                text = element_text(size = 12), 
                axis.text.y = element_text(size = 12),
                axis.title = element_text(size = 12.5), 
                plot.title = element_text(size = 14))
  })
  # Combine plots
  plots <- ggpubr::ggarrange(plotlist = plots, ncol = 2, nrow=3)
  return(plots)
}
