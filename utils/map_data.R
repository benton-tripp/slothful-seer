# Map the data
map.data <- function(df, 
                     rasters, 
                     .raster=as.character(seq(1, 11)), 
                     title="") {
  
  if (is.null(.raster)) {
    .raster <- .raster %>% 
      match.arg() %>% 
      as.integer()
  }
  
  # Raster overlay
  overlay <- rasters[[.raster]]
  
  # Find the middle point of the raster
  center <- c(x=mean(df$lon), 
              y=mean(df$lat))
  
  # Get finite values range
  finite_values <- raster::values(overlay)[is.finite(raster::values(overlay))]
  
  # Use this range in colorNumeric()
  color.palette <- colorNumeric(palette = "viridis", 
                                domain = range(finite_values),
                                na.color = "#00000000") 
  
  leaflet() %>%
    addTiles() %>%  
    addRasterImage(overlay, colors = color.palette) %>%
    addCircleMarkers(lat=~lat, lng=~lon, data=df %>% filter(presence == 0), 
                     color="black", radius=0.5, group="Absence") %>%
    addCircleMarkers(lat=~lat, lng=~lon, data=df %>% filter(presence == 1), 
                     color="red", radius=0.5, group="Presence") %>%
    addLayersControl(overlayGroups = c("Presence", "Absence")) %>%
    addControl(html = paste("<h4>", title, "</h4>"), position = "topleft") %>%
    fitBounds(lng1 = min(df$lon) - .5, lat1 = min(df$lat) - .5, 
              lng2 = max(df$lon) + .5, lat2 = max(df$lat) + .5) %>% 
    addLegend("bottomright", 
              colors = c("red", "black"), 
              labels = c("Presence", "Absence"), 
              title = "Observations")
}