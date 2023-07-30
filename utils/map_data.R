# Function to convert CRS to a leafletCRS object
convert.CRS <- function(raster.crs) {
  # Get the CRS in PROJ.4 format from the raster
  proj4.string <- raster::proj4string(raster.crs)
  # Convert the CRS to a leafletCRS object
  leafletCRS(proj4def = proj4.string)
}

# Map the data
map.data <- function(df, title="", points=T, max.bounds=NULL, hover=F, raster.crs=NULL) {
  
  # Find the middle point of the map
  center <- c(x=mean(df$lon), 
              y=mean(df$lat))
  
  if (!is.null(raster.crs)) {
    leaflet.options <- leafletOptions(
      crs=convert.CRS(raster.crs),
      minZoom = 3.5,  # Set the minimum zoom level 
      maxZoom = 18,  # Set the maximum zoom level 
      worldCopyJump = F  # Set to FALSE to prevent the map from wrapping around the world
    )
  } else {
    leaflet.options <- leafletOptions(
      minZoom = 3.5,  # Set the minimum zoom level 
      maxZoom = 18,  # Set the maximum zoom level 
      worldCopyJump = F  # Set to FALSE to prevent the map from wrapping around the world
    )
  }
  
  plt <- leaflet(options=leaflet.options) %>%
    addTiles() 
  if (points) {
    plt <- plt %>% 
      addCircleMarkers(lat=~lat, lng=~lon, data=df %>% filter(presence == 1), 
                     color="darkred", radius=0.6)
  }
  if (!is.null(title) & title != "") {
    plt <- plt %>%
      addControl(html = paste("<h4>", title, "</h4>"), position = "bottomright")
  }
  if (!is.null(max.bounds)) {
    plt <- plt %>%
      setMaxBounds(lng1 = max.bounds[1], lat1 = max.bounds[2], lng2 = max.bounds[3], lat2 = max.bounds[4])
  } else {
    plt <- plt %>%
      setMaxBounds(lng1 = min(df$lon) - .5, lat1 = min(df$lat) - .5, 
                   lng2 = max(df$lon) + .5, lat2 = max(df$lat) + .5)
  }
  if (hover) {
    # Source: https://shiny.posit.co/r/articles/build/js-send-message/
    plt <- plt %>% onRender(
      "function(el, x) {
                    var clickedCoordinates = null;
                    var map = this;
                    function addMarker(lat, lng) {
                      if (clickedCoordinates) {
                        map.removeLayer(clickedCoordinates);
                      }
                      clickedCoordinates = L.circleMarker([lat, lng], 
                        { color: 'blue', radius: 5 }).addTo(map);
                    }
                    this.on('mousemove', function(e) {
                        var lat = e.latlng.lat;
                        var lng = e.latlng.lng;
                        var coord = [lat, lng];
                        Shiny.onInputChange('hover_coordinates', coord)
                    });
                    this.on('mouseout', function(e) {
                        Shiny.onInputChange('hover_coordinates', null)
                    });
                    this.on('click', function(e) {
                        var lat = e.latlng.lat;
                        var lng = e.latlng.lng;
                        addMarker(lat, lng);
                        var coord = [lat, lng];
                        Shiny.onInputChange('click_coordinates', coord)
                    });
                }"
    )
  }
  plt
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
