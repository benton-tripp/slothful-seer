cat("Loading data...\n")

.file.check <- list(
  bradypus = file.exists("data/bradypus.rds"),
  rasters = file.exists("data/rasters.rds"),
  binary.rasters = file.exists("data/binary.rasters.rds"),
  pseudo.absence = file.exists("data/pseudo.absence.df.rds"),
  presence = file.exists("data/presence.df.rds")
)

# Function to generate binary raster for a given biome
create.binary.raster <- function(biome.value, biome.categories, .ras, rasters) {
  # cat("Biome Value: ", biome.value, "\n")
  # Create a reclassification matrix
  # For the current biome category, set a range of [biome.value, biome.value]
  # For the other categories, set a range of [min(other), max(other)]
  other.categories <- biome.categories[biome.categories != biome.value]
  # cat("Other Categories: ", paste(other.categories, collapse=", "), "\n")
  reclass.matrix <- matrix(c(biome.value, 1), ncol=2, byrow=T) %>%
    rbind(
      purrr::map_vec(other.categories, ~matrix(c(.x, 0), ncol=2, byrow=T))
    )
  
  # Reclassify the biome raster
  binary.raster <- reclassify(.ras, rcl = reclass.matrix)
  
  # Set NA values
  binary.raster[is.na(rasters[[1]])] <- NA
  
  return(binary.raster)
}

cat("Loading world map...\n")
# Load South America data
data("wrld_simpl")
south.america <- subset(wrld_simpl, wrld_simpl$SUBREGION==5 | wrld_simpl$SUBREGION==13)

raster.names <- c(
  "mean.temp",
  "prec.annual",
  "prec.wet.qrtr",
  "prec.dry.qrtr",
  "max.temp",
  "min.temp",
  "temp.rng",
  "mean.tmp.wet.qrtr",
  "biome", # https://www.worldwildlife.org/publications/terrestrial-biomeions-of-the-world 
  "lon",
  "lat"
) 

cat("Loading bradypus data...\n")
# Get sloth data
if (.file.check$bradypus) {
  bradypus <- readRDS("data/bradypus.rds")
} else {
  bradypus <- gbif("Bradypus", "variegatus*", sp=T)
  # Assign initial CRS - WGS 84 (EPSG: 4326)
  sp::proj4string(bradypus) <- sp::CRS("+init=epsg:4326") 
  
  # Reproject bradypus points to match the crs of South America
  bradypus <- sp::spTransform(bradypus, CRS=crs(south.america))
  
  # Reproject the bradypus points
  bradypus <- sp::spTransform(bradypus, CRS=crs(south.america))
  saveRDS(bradypus, "data/bradypus.rds")
}

cat("Loading rasters...\n")
# Get rasters data
if (.file.check$rasters) {
  rasters <- readRDS("data/rasters.rds")
} else {
  files <- list.files(path=paste("data", sep=''), pattern='grd', full.names=T)
  rasters <- stack(files)
  
  # Create a raster for longitude
  lon.raster <- rasters[[1]]
  values(lon.raster) <- xFromCell(lon.raster, seq_len(ncell(lon.raster)))
  lon.raster[is.na(rasters[[1]])] <- NA
  
  # Create a raster for latitude
  lat.raster <- rasters[[1]]
  values(lat.raster) <- yFromCell(lat.raster, seq_len(ncell(lat.raster)))
  lat.raster[is.na(rasters[[1]])] <- NA
  
  rasters <- stack(rasters, lon.raster, lat.raster)
  names(rasters) <- raster.names
  
  # Fix crs for raster stack
  crs(rasters) <- crs(south.america)
  
  # Set extent to be South America
  extent(rasters) <- extent(south.america)
  
  saveRDS(rasters, "data/rasters.rds")
}

# Get unique biome categories
biome.categories <- unique(values(rasters$biome))
biome.categories <- biome.categories[!is.na(biome.categories)]

cat("Biome categories:", paste(biome.categories, collapse=", "), "\n")
cat("Creating binary rasters from biome raster...\n")

# Get binary rasters
if (.file.check$binary.rasters) {
  binary.rasters <- readRDS("data/binary.rasters.rds")
} else {
  # Apply the function to each biome category
  binary.rasters <- map(biome.categories, 
                        function(.x) {
                          cat("Creating raster for", .x, "biome...\n")
                          create.binary.raster(.x, biome.categories, 
                                               rasters$biome, rasters)
                        })
  
  # Name the rasters
  names(binary.rasters) <- paste0("biome", biome.categories)
  
  # Stack the rasters together
  binary.rasters <- stack(binary.rasters)
  
  # Add to original rasters stack
  binary.rasters <- stack(subset(rasters, raster.names[raster.names != "biome"]), 
                          binary.rasters)
  
  # Check for all NA values
  # any(purrr::map_lgl(seq(1,13), ~all(is.na(as.matrix(binary.rasters[[.x]])))))
  
  saveRDS(binary.rasters, "data/binary.rasters.rds")
}

cat("Getting presence data...\n")
# Presence values
if (.file.check$presence) {
  presence.df <- readRDS("data/presence.df.rds")
} else {
  presence.df <- raster::extract(rasters, bradypus) %>%
    as.data.frame() %>%
    dplyr::select(-lat, -lon) %>%
    cbind(
      bradypus %>% 
        as.data.frame() %>% 
        dplyr::select(lon, lat)
    ) %>%
    mutate(biome = as.factor(biome),
           presence = 1) %>%
    mutate(na.vals = if_any(names(rasters), ~is.na(.x))) %>%
    filter(!na.vals) %>%
    dplyr::select(-na.vals)
  saveRDS(presence.df, "data/presence.df.rds")
}

# Get presence.df points as spdf
sp.presence <- presence.df %>%
  sp::SpatialPointsDataFrame(coords = .[, c("lon", "lat")], 
                             data = ., proj4string = CRS(sp::proj4string(bradypus)))

cat("Getting absence data...\n")
# Create random points on cells of the environmental rasters within the 
# extent of bradypus, and avoiding cells containing points from bradypus;
# These will be used as pseudo-absence points
if (.file.check$pseudo.absence) {
  pseudo.absence.df <- readRDS("data/pseudo.absence.df.rds")
} else {
  
  pseudo.absence <- randomPoints(rasters, n=nrow(sp.presence), p=sp.presence, 
                                 ext=extent(sp.presence), extf=1.01) %>%
    SpatialPoints(crs(sp.presence))
  
  pseudo.absence <- sp::spTransform(pseudo.absence, CRS=crs(south.america))
  
  pseudo.absence.df <- pseudo.absence %>%
    raster::extract(rasters, .) %>% 
    as.data.frame() %>% 
    dplyr::select(-lat, -lon) %>% 
    cbind(
      pseudo.absence %>%
        as.data.frame() %>%
        dplyr::select(x, y) %>% 
        rename(lon=x, lat=y)
    ) %>%
    mutate(biome = as.factor(biome),
           presence = 0) %>%
    mutate(na.vals = if_any(names(rasters), ~is.na(.x))) %>%
    filter(!na.vals) %>%
    dplyr::select(-na.vals)
  
  pseudo.absence.df <- pseudo.absence.df %>%
    filter(., if_any(names(rasters), ~!is.na(.x))) 
  
  saveRDS(pseudo.absence.df, "data/pseudo.absence.df.rds")
}

# Crop rasters to extent of presence points (plus small buffer)
buffered.extent <- extent(sp.presence) + c(-1, 1, -1, 1)
rasters <- raster::crop(rasters, buffered.extent)
binary.rasters <- raster::crop(binary.rasters, buffered.extent)

# Combine presence data with pseudo-absence data
df <- rbind(presence.df, pseudo.absence.df) %>% arrange(presence)

cat("Completed data loading process\n")