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

get.study.area <- function() {
  # Load South America data
  data("wrld_simpl")
  
  # Convert to sf object
  south.america.sf <- sf::st_as_sf(wrld_simpl)
  
  # Assuming you have a raster object named rasters, get the extent
  study.extent <- extent(rasters[[1]])
  
  # Extract the bounding box coordinates
  xmin <- study.extent@xmin
  xmax <- study.extent@xmax
  ymin <- study.extent@ymin
  ymax <- study.extent@ymax
  
  # Plot
  ggplot() +
    geom_sf(data = south.america.sf, fill = "lightblue") +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
              color = "red", fill = NA, linewidth = 1) +
    coord_sf(xlim = c(-100, -20), ylim = c(-60, 20)) + 
    theme_bw() +
    theme(axis.text.y = element_text(size = 13),
          axis.text.x = element_text(size = 13),
          panel.grid.major = element_line(color = "#aaaaaa"))
}

# It's a lot faster to load from an image, so create one if you haven't already
if (!file.exists("www/images/study_area.png")) {
  # Load study area map
  study.area.plt <- get.study.area()
  ggplot2::ggsave("www/images/study_area.png", study.area.plt, 
                  width = 7, height = 7, units = "in")
}
