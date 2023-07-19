# Map the data
map.data <- function(df, title="") {
  
  # Find the middle point of the map
  center <- c(x=mean(df$lon), 
              y=mean(df$lat))
  
  leaflet() %>%
    addTiles() %>%  
    addCircleMarkers(lat=~lat, lng=~lon, data=df %>% filter(presence == 1), 
                     color="darkred", radius=0.6) %>%
    addControl(html = paste("<h4>", title, "</h4>"), position = "bottomright") %>%
    fitBounds(lng1 = min(df$lon) - .5, lat1 = min(df$lat) - .5, 
              lng2 = max(df$lon) + .5, lat2 = max(df$lat) + .5) 
}