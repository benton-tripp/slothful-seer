# IPP 
predict.ipp <- function(model.outputs, raster.imgs, rasters, model.vars, cutoff) {
  
  # Use predict.ppm to generate predicted intensities across rasters
  predicted.intensities <- predict.ppm(model.outputs$ipp, covariates=raster.imgs)
  intensity.values <- as.matrix(predicted.intensities) %>% 
    reduce(c) %>% 
    keep(~!is.na(.x))
  
  # Convert the im object to a raster
  predicted.raster <- raster(predicted.intensities)
  
  # If there is an intensity (count) of at least one, then predict it as a probability of 1
  # Calculate the probability of finding at least one sloth at a location as 
  # 1 - exp(-λ), where λ is the predicted intensity. This calculation is based 
  # on the cumulative distribution function of the Poisson distribution. I.e.,
  # the probability of observing zero events in a Poisson dist. is given by:
  # P(X = 0) = exp(-λ), and so P(X≥1)=1−P(X=0)=1−exp(−λ)
  prob.at.least.1 <- calc(predicted.raster, function(x) {1 - exp(-x)})
  crs(prob.at.least.1) <- crs(rasters[[1]])
  
  # Extract the predicted probabilities for the test points
  ipp.probs <- raster::extract(prob.at.least.1, model.vars$train.test.data$test[, c("lon", "lat")])
  
  ipp.yhat <- factor(ifelse(ipp.probs >= cutoff, 
                            "presence", "no.presence"), 
                     levels=c("no.presence", "presence"))
  
  y <- factor(ifelse(model.vars$train.test.data$test$presence == 1, 
                     "presence", "no.presence"), 
              levels=c("no.presence", "presence"))
  
  # Get confusion matrix
  ipp.cm <- confusionMatrix(
    ipp.yhat, 
    y,
    mode="everything",
    positive="presence")
  
  test.df <- data.frame(model.vars$train.test.data$test[, c("lon", "lat")]) %>%
    mutate(yhat = ipp.yhat,
           y = y)
  
  # Output
  list(raster = prob.at.least.1, cm = ipp.cm, y = y, yhat = ipp.yhat, 
       probs = ipp.probs, test = test.df)
}

predict.evaluate.maxent <- function(model.outputs, rasters, model.vars, cutoff) {
  
  # Make predictions on full raster
  maxent.raster <- dismo::predict(model.outputs$maxent, x=rasters)
  crs(maxent.raster) <- crs(rasters[[1]])
  
  # Make predictions on the test data
  test.x <- model.vars$train.test.data$test %>% 
    dplyr::select(-c("presence"))
  test.y <- model.vars$train.test.data$test$presence
  
  # Get predictions using test set
  maxent.probs <- predict(model.outputs$maxent, x=test.x)
  
  maxent.yhat <- factor(ifelse(maxent.probs >= cutoff, 
                            "presence", "no.presence"), 
                     levels=c("no.presence", "presence"))
  
  y <- factor(ifelse(test.y == 1, 
                     "presence", "no.presence"), 
              levels=c("no.presence", "presence"))
  
  # Create a confusion matrix for the maxent model
  maxent.cm <- confusionMatrix(
    maxent.yhat, y,
    mode="everything",
    positive = "presence")
  
  # Make predictions on full raster
  maxent.raster <- dismo::predict(model.outputs$maxent, x=rasters)
  crs(maxent.raster) <- crs(rasters[[1]])
  
  test.df <- data.frame(test.x[, c("lon", "lat")]) %>%
    mutate(yhat = maxent.yhat,
           y = y)
  
  # Output
  list(raster = maxent.raster, cm = maxent.cm, y = y, yhat = maxent.yhat, 
       probs = maxent.probs, test = test.df)
}

predict.evaluate.glm <- function(model.outputs, binary.rasters, model.vars, cutoff) {
  # Generate predictions on rasters (to plot probabilities)
  glm.raster <- 1 - raster::predict(object=binary.rasters, 
                                    model=model.outputs$glm, type="prob")
  
  # Generate predictions test set
  glm.probs <- predict(model.outputs$glm, 
                      newdata = model.vars$train.test.data$test.dummies, 
                      type = "prob")
  
  # Apply custom cutoff to get predicted classes
  glm.yhat <- factor(ifelse(glm.probs[, "presence"] >= cutoff, "presence", "no.presence"),
                     levels=levels(model.vars$train.test.data$test.dummies$presence))
  
  y <- model.vars$train.test.data$test.dummies$presence
  
  # Compute metrics using confusion matrix
  glm.cm <- confusionMatrix(
    glm.yhat, y, 
    positive="presence",
    mode="everything")
  
  test.df <- data.frame(model.vars$train.test.data$test.dummies[, c("lon", "lat")]) %>%
    mutate(yhat = glm.yhat,
           y = y)
  
  # Output
  list(raster = glm.raster, cm = glm.cm, y = y, yhat = glm.yhat, 
       probs=glm.probs, test=test.df)
}


predict.evaluate.tree <- function(model.outputs, rasters, model.vars, cutoff) {
  # Generate predictions on rasters (to plot probabilities)
  ct.raster <- 1 - raster::predict(
    object=rasters, model=model.outputs$ct, type="prob", 
    factors=list(biome=levels(model.vars$train.test.data$test.factor$biome)))
  
  # Get predictions
  # Generate predictions test set
  ct.probs <- predict(model.outputs$ct, 
                       newdata = model.vars$train.test.data$test.factor, 
                       type = "prob")
  
  # Apply custom cutoff to get predicted classes
  ct.yhat <- factor(ifelse(ct.probs[, "presence"] >= cutoff, "presence", "no.presence"),
                     levels=levels(model.vars$train.test.data$test.factor$presence))
  y <- model.vars$train.test.data$test.factor$presence
  
  # Compute metrics using confusion matrix
  ct.cm <- confusionMatrix(
    ct.yhat, y, 
    positive="presence",
    mode="everything")
  
  test.df <- data.frame(model.vars$train.test.data$test.factor[, c("lon", "lat")]) %>%
    mutate(yhat = ct.yhat,
           y = y)
  
  # Output
  list(raster = ct.raster, cm = ct.cm, y = y, yhat = ct.yhat, 
       probs = ct.probs, test = test.df)
}

predict.evaluate.rf <- function(model.outputs, rasters, model.vars, cutoff) {
  # Generate predictions on rasters (to plot probabilities)
  rf.raster <- 1 - raster::predict(
    object=rasters, model=model.outputs$rf, type="prob", factors=list(biome=levels(df$biome)))
  
  # Generate predictions test set
  rf.probs <- predict(model.outputs$rf, 
                      newdata = model.vars$train.test.data$test.factor, 
                      type = "prob")
  
  # Apply custom cutoff to get predicted classes
  rf.yhat <- factor(ifelse(rf.probs[, "presence"] >= cutoff, "presence", "no.presence"),
                    levels=levels(model.vars$train.test.data$test.factor$presence))
  y <- model.vars$train.test.data$test.factor$presence 
  
  # Compute metrics using confusion matrix
  rf.cm <- confusionMatrix(
    rf.yhat, y, 
    positive="presence",
    mode="everything")
  
  test.df <- data.frame(model.vars$train.test.data$test.factor[, c("lon", "lat")]) %>%
    mutate(yhat = rf.yhat,
           y = y)
  
  # Output
  list(raster = rf.raster, cm = rf.cm, y = y, yhat = rf.yhat, 
       probs = rf.probs, test = test.df)
}

