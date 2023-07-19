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

prepare.data <- function(df, p) {
  train.index <- stratified.split.idx(df, p=p)
  df.train <- df[train.index, ]
  df.test <- df[-train.index, ]
  
  presence.df.train <- df.train %>% filter(presence == 1)
  presence.df.test <- df.test %>% filter(presence == 1)
  
  list(train = df.train, test = df.test, presence.train = presence.df.train, presence.test = presence.df.test)
}

convert.biomes.to.dummy <- function(df.train, df.test) {
  dummy.biome.train <- model.matrix(~biome-1, data=df.train)
  dummy.biome.test <- model.matrix(~biome-1, data=df.test)
  
  df.train.dummies <- cbind(dplyr::select(df.train, -biome), dummy.biome.train)
  df.test.dummies <- cbind(dplyr::select(df.test, -biome), dummy.biome.test)
  
  list(train.dummies = df.train.dummies, test.dummies = df.test.dummies)
}

