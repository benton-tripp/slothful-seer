stratified.split.idx <- function(df, p=0.75, lat.lon.bins=10) {
  # Cut along lat/lon values to create grids (lat.bin & lon.bin)
  # lat.lon.bins is the number of divisions you want
  df$lat.bin <- cut(df$lat, breaks=lat.lon.bins, labels = F)
  df$lon.bin <- cut(df$lon, breaks=lat.lon.bins, labels = F)
  
  # Create a new variable combining the stratification variables:
  # - lat.bin
  # - lon.bin
  # - presence
  # - biome
  df %>%
    mutate(strata = paste(lat.bin, lon.bin, presence, biome, sep = "|")) %>%
    pull(strata) %>%
    # Create the data partitions
    createDataPartition(., p = p, list = F) %>%
    suppressWarnings()
}