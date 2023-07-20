train.model <- function(
    df, 
    train.grid=NULL, 
    train.control=NULL, 
    rasters=NULL,
    full.df=NULL,
    model.type=c("glm", "tree", "rf", "ipp", "maxent"),
    cache=T, 
    file.loc="data/model_cache/model.rds"
) {
  # Match model type argument
  model.type <- match.arg(model.type)
  
  # First try to load from model cache
  if (file.exists(file.loc)) {
    cat("Reading", model.type, "model from", file.loc, "\n")
    return(readRDS(file.loc))
  }
  
  # If file not in cache, fit model given the type
  
  # Logistic Regression
  if (model.type == "glm") {
    fit <- train(presence ~ ., 
                 data = df, 
                 method = "glmnet", 
                 family = "binomial",
                 preProcess = c("center", "scale"),
                 trControl = train.control, 
                 tuneGrid = train.grid,
                 metric = "Accuracy")
    # Classification Tree or Random Forest
  } else if (model.type %in% c("tree", "rf")) {
    if (model.type == "rf") {
      y <- factor(ifelse(df$presence == "1", "presence", "no.presence"), 
                  levels=c("no.presence", "presence"))
    } else {
      y <- df$presence
    }
    
    fit <- train(x = df %>% dplyr::select(-presence), 
                 y = y, 
                 method = ifelse(model.type == "tree", "rpart", "ranger"),
                 trControl = train.control, 
                 tuneGrid = train.grid,
                 metric = "Accuracy")
    # Inhomogenous Poisson Process
  } else if (model.type == "ipp") {
    # Convert the data to a ppp object
    locations <- ppp(df$lon, df$lat, 
                     window=owin(range(full.df$lon), 
                                 range(full.df$lat)))
    
    # Initialize a list to hold the binned rasters
    raster.names <- names(rasters)[!(names(rasters) %in% c("lat", "lon"))]
    raster.imgs <- map(raster.names, ~as.im(rasters[[.x]]))
    names(raster.imgs) <- raster.names
    
    # Create formula
    .f <- formula(paste0("locations ~ ", paste(raster.names, collapse = " + ")))
    
    # Fit the IPP model
    fit <- ppm(.f, covariates=raster.imgs)
    
    # Maximum Entropy
  } else if (model.type == "maxent") {
    # Transform data into the format needed for Maxent
    train.x <- df %>% 
      dplyr::select(-c("presence")) 
    train.y <- df$presence
    
    # Create Maxent Model
    fit <- maxent(x=train.x, p=train.y, factors="biome")
    
  }
  # Cache the model
  if (cache) {
    cat("Saving", model.type, "model to", file.loc, "\n")
    saveRDS(list(fit=fit, cache=file.loc), file.loc)
    return(list(fit=fit, cache=file.loc))
  }
  return(list(fit=fit, cache=NULL))
}