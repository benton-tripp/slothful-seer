# Load libraries
purrr::walk(
  c("shiny", "dismo", "sp", "maptools", "tidyverse", "ggpubr", "skimr", "knitr", "shinyjs",
    "dismo", "leaflet", "leaflet.extras", "raster", "spatstat", "maxnet", "caret", "DT"), 
  ~suppressWarnings(suppressPackageStartupMessages(library(.x, character.only=T)))
)

# Set seed for reproducibility
set.seed(19)

# Source utility functions (e.g., get_data.R loads the data)
.utils <- file.path("utils", list.files("utils"))
walk(.utils, ~source(.x)) %>% suppressWarnings()


function(input, output, session) {
  
  ### Data Overview ###############################
  
  # Data
  observeEvent(c(input$sidebarMenu, input$select_data_filter), {
    if (input$sidebarMenu == "dataPanel") { 
      if (input$select_data_filter == "Presence") {
        data.selection <- presence.df
      } else if (input$select_data_filter == "Absence") {
        data.selection <- pseudo.absence.df
      } else {
        data.selection <- df
      }
      output$all_data_selection <- renderDT({
        datatable(
          data.selection %>% mutate(lat=round(lat,2), lon=round(lon,2)),
          rownames=F,
          selection='none',
          options=list(
            paging=T,
            scrollX=T,
            scrollY=F,
            searching=T,
            orderMulti=T,
            info=T,
            lengthChange=T,
            pageLength=10
          )
        ) %>%
          formatStyle(columns=names(data.selection), `font-size`="18px")
      })
    }
  })
  
  ### Exploratory Analysis ########################
  
  
  
  # Map
  observeEvent(c(input$edaTabs, input$map_type, input$map_selection), {
    if (input$edaTabs == "Maps") {
      if (input$map_type == "Interactive Map of Points") {
        output$map <- renderUI({map.data(df, title="Three-Toed-Sloths")})
      } else {
        map.input <- case_when(
          input$map_selection == "Mean Temperature" ~ 1,
          input$map_selection == "Annual Precipitation" ~ 2,
          input$map_selection == "Precipitation - Wettest Qrtr." ~ 3,
          input$map_selection == "Precipitation - Driest Qrtr." ~ 4,
          input$map_selection == "Max Temperature" ~ 5,
          input$map_selection == "Min Temperature" ~ 6,
          input$map_selection == "Temperature Range" ~ 7,
          input$map_selection == "Mean Temperature - Wettest Qrtr." ~ 8,
          input$map_selection == "Biomes" ~ 9,
          input$map_selection == "Longitude" ~ 10,
          T ~ 11)
        output$raster_plot <- renderPlot({
          # Define a vector of colors for each factor level
          colors <- c("0" = "black", "1" = "red")
          plot(rasters[[map.input]])
          if (input$map_absence == "Both") {
            points(df$lon, df$lat, col = colors[factor(df$presence, levels=c(0, 1))], pch = 20, cex = 0.4) 
          } else if (input$map_absence == "Presence") {
            points(presence.df$lon, presence.df$lat, col = "red", pch = 20, cex = 0.4) 
          } else if (input$map_absence == "Absence") {
            points(pseudo.absence.df$lon, pseudo.absence.df$lat, col = "black", pch = 20, cex = 0.4) 
          }
          
        }, width=500, height=500)
      }
    }
  })
  
  
  # Continuous Data
  cont.data <- cont.eda(df, "All", "Table")
  cont.datatable <- format.datatable(cont.data, signif=4, searching=F, 
                                     font.size="17px", page.length=nrow(cont.data))
  updateSelectInput(session=session,
                    inputId="continuous_var", 
                    choices=c(names(df)[!(names(df) %in% c("presence", "biome"))], "All"))
  
  observeEvent(c(input$edaTabs, input$cont_plot_type), {
    if (input$edaTabs == "Continuous Data") {
      # Table
      if (input$cont_plot_type != "Table") {
        output$cont_plot <- renderPlot(cont.eda(df, input$continuous_var, 
                                                input$cont_plot_type), width=800)
        output$cont_table <- NULL
      } else {
        output$cont_plot <- NULL
        output$cont_table <- renderDT(cont.datatable)
      }
      # Plot
      
    }
  })
  
  # Categorical data
  cat.data <- cat.eda(df, "Table") %>% arrange(-n)
  cat.datatable <- format.datatable(cat.data, page.length=nrow(cat.data), 
                                    font.size="19px", searching=F)
  
  observeEvent(c(input$edaTabs, input$cat_plot_type), {
    if (input$edaTabs == "Categorical Data") {
      # Table
      output$cat_table <- renderDT(cat.datatable)
      # Plot
      output$cat_plot <- renderPlot(cat.eda(df, input$cat_plot_type), width=500)
    }
  })
  
  # Presence Data 
  pres.datatable <- presence.eda(df, "Table")
  observeEvent(c(input$edaTabs, input$pres_plot_type), {
    if (input$edaTabs == "Presence/Absence") {
      # Table
      output$pres_table <- renderDT(pres.datatable)
      # Plot
      output$pres_plot <- renderPlot(presence.eda(df, input$pres_plot_type))
    }
  })
  
  ### Modeling Fitting #######################################
  
  # Initialize Model Info Page Menu default selection
  runjs(
    "Shiny.setInputValue(\"menuItemSelected\", \"mdlOverviewSelect\", {priority: 'event'});"
  )
  
  # Store the previous values of the model parameters
  prev.model.inputs <- reactiveVal({
    list(
      p=0.75,
      k.folds=5,
      alpha.1=0,
      alpha.2=NA,
      alpha.3=NA,
      lambda.1=0.0,
      lambda.2=NA,
      lambda.3=NA,
      cp.start=0,
      cp.range=1,
      cp.step=0.1,
      split.rule="gini", 
      mtry.1=1,
      min.node.1=1,
      mtry.2=NA,
      min.node.2=NA,
      mtry.3=NA,
      min.node.3=NA
    )
  })
  
  observeEvent(input$undo_model_updates, {
    # When 'Undo Changes' is clicked, revert to the previous values
    prevVals <- prev.model.inputs()
    
    # Use `update*` functions to change the inputs
    updateNumericInput(session, "split_perc", value = prevVals$p)
    updateNumericInput(session, "k_fold", value=prevVals$k.fold)
    updateNumericInput(session, "glm_alpha_1", value = prevVals$alpha.1)
    updateNumericInput(session, "glm_alpha_2", value = prevVals$alpha.2)
    updateNumericInput(session, "glm_alpha_3", value = prevVals$alpha.3)
    updateNumericInput(session, "glm_lambda_1", value = prevVals$lambda.1)
    updateNumericInput(session, "glm_lambda_2", value = prevVals$lambda.2)
    updateNumericInput(session, "glm_lambda_3", value = prevVals$lambda.3)
    updateNumericInput(session, "start_cp", value = prevVals$cp.start)
    updateNumericInput(session, "range_cp", value = prevVals$cp.range)
    updateNumericInput(session, "step_cp", value = prevVals$cp.step)
    updateSelectInput(session, "split_rule", selected = prevVals$split.rule)
    updateNumericInput(session, "mtry_1", value = prevVals$mtry.1)
    updateNumericInput(session, "min_node_1", value = prevVals$min.node.1)
    updateNumericInput(session, "mtry_2", value = prevVals$mtry.2)
    updateNumericInput(session, "min_node_2", value = prevVals$min.node.2)
    updateNumericInput(session, "mtry_3", value = prevVals$mtry.3)
    updateNumericInput(session, "min_node_3", value = prevVals$min.node.3)
    
    # Reset train/test split data
    # TODO: You might need additional code/logic to reset train/test split data
  })
  
  observeEvent(input$reset_model_updates, {
    # When 'Reset to Default' is clicked, revert to the default values
    updateNumericInput(session, "split_perc", value = 0.75)
    updateNumericInput(session, "k_folds", value = 5)
    updateNumericInput(session, "glm_alpha_1", value = 0.0)
    updateNumericInput(session, "glm_alpha_2", value = NA)
    updateNumericInput(session, "glm_alpha_3", value = NA)
    updateNumericInput(session, "glm_lambda_1", value = 0.0)
    updateNumericInput(session, "glm_lambda_2", value = NA)
    updateNumericInput(session, "glm_lambda_3", value = NA)
    updateNumericInput(session, "step_cp", value=0.1)
    updateNumericInput(session, "range_cp", value=1)
    updateNumericInput(session, "start_cp", value=0)
    updateSelectInput(session, "split_rule", selected = "gini")
    updateNumericInput(session, "mtry_1", value = 1)
    updateNumericInput(session, "min_node_1", value = 1)
    updateNumericInput(session, "mtry_2", value = NA)
    updateNumericInput(session, "min_node_2", value = NA)
    updateNumericInput(session, "mtry_3", value = NA)
    updateNumericInput(session, "min_node_3", value = NA)
    
    # Reset train/test split data
    # TODO: You might need additional code/logic to reset train/test split data
  })
  
  model.vars <- reactive({
    if (input$modelTabs == "Model Fitting") {
      if (any(purrr::map_lgl(
        c(input$split_perc, input$k_folds, input$glm_alpha_1, 
          input$glm_lambda_1, input$start_cp, input$range_cp, 
          input$step_cp, input$mtry_1, input$min_node_1),
        ~(is.null(.x) | is.na(.x) | !is.numeric(.x))
      )) | is.null(input$split_rule)) {
        showNotification(tags$span("One or more of the required values is missing"), type="error")
        return(NULL)
      } 
      # Prepare Data
      prepared.data <- tryCatch(prepare.data(df, input$split_perc), 
                                error = function(e) {
                                  showNotification(paste0("Data preparation error: ", 
                                                          e$message), type = "error")
                                  return(NULL)
                                })
      
      if (is.null(prepared.data)) return(NULL)
      
      # Convert Biomes to Binary Dummy Variables
      dummy.data <- convert.biomes.to.dummy(prepared.data$train.factor, 
                                            prepared.data$test.factor)
      
      # Error-check for cp
      cp.seq <- tryCatch(
        {seq_len(input$range_cp) * input$step_cp + input$start_cp},
        warning = function(w) {
          showNotification(paste0("CP sequence warning: ", w$message), 
                           type = "warning")
          return(NULL)
        },
        error = function(e) {
          showNotification(paste0("CP sequence error: ", e$message), type = "error")
          return(NULL)
        })
      
      if (any(purrr::map_lgl(
        clean.inputs(c(input$mtry_1, input$mtry_2, input$mtry_3)), 
        ~.x > ncol(df) - 1))
      ) {
        showNotification(paste0("Error in Random Forest grid input: `mtry` cannot be greater",
                                " than the number of predictor fields in the training data (i.e., ", 
                                ncol(df) - 1, ")"), type = "error")
        return(NULL)
      }
      
      if (any(purrr::map_lgl(
        clean.inputs(c(input$alpha_1, input$alpha_2, input$alpha_3)), 
        ~.x > 1 | .x < 0))
      ) {
        showNotification(paste0("Error in GLM grid input: `alpha` cannot be greater than 1",
                                " or less than 0."), type = "error")
        return(NULL)
      }
      
      if (any(purrr::map_lgl(
        clean.inputs(c(input$lambda_1, input$lambda_2, input$lambda_3)), 
        ~.x < 0))
      ) {
        showNotification(paste0("Error in GLM grid input: `lambda` cannot be less than 0."), 
                         type = "error")
        return(NULL)
      }
      
      if (is.null(cp.seq)) return(NULL)
      
      # Extract model inputs and prepare data for modeling
      model.inputs <- list(
        p=input$split_perc,
        k.folds=input$k_folds,
        alpha.1=input$glm_alpha_1,
        alpha.2=input$glm_alpha_2,
        alpha.3=input$glm_alpha_3,
        lambda.1=input$glm_lambda_1,
        lambda.2=input$glm_lambda_2,
        lambda.3=input$glm_lambda_3,
        cp=cp.seq,
        split.rule=input$split_rule, 
        mtry.1=input$mtry_1,
        min.node.1=input$min_node_1,
        mtry.2=input$mtry_2,
        min.node.2=input$min_node_2,
        mtry.3=input$mtry_3,
        min.node.3=input$min_node_3
      )
      
      # Combine results into a list and return
      return(list(
        model.variables = model.inputs,
        train.test.data = c(prepared.data, dummy.data)
      ))
    }
  }) %>% bindEvent(input$apply_model_updates) # Only update when "apply" is clicked
  
  # Initialize model input list
  model.inputs <- list()
  
  observeEvent(model.vars(), {
    if (!is.null(model.vars())) {
      # Reset model input list
      model.inputs <<- list()
      
      # This observer will run when `model.vars` gets updated and is not NULL
      .inputs <- model.vars()$model.variables
      
      # Define training cv control
      control.class.probs <- trainControl(method="cv", number=.inputs$k.folds, classProbs = T)
      control <- trainControl(method="cv", number=.inputs$k.folds)
      
      # Create model training grids
      
      # GLM Grid
      alpha <- clean.inputs(c(.inputs$alpha.1, .inputs$alpha.2, .inputs$alpha.3))
      lambda <- clean.inputs(c(.inputs$lambda.1, .inputs$lambda.2, .inputs$lambda.3))
      glm.grid <- expand.grid(
        alpha=alpha,
        lambda=lambda
      )
      
      # Tree Grid
      cp.seq <- clean.inputs(.inputs$cp)
      ct.grid <- data.frame(cp=cp.seq)
      
      # Random Forest Grid
      split.rule <- clean.inputs(.inputs$split.rule)
      mtry <- clean.inputs(c(.inputs$mtry.1, .inputs$mtry.2, .inputs$mtry.3))
      min.node <- clean.inputs(c(.inputs$min.node.1, .inputs$min.node.2, .inputs$min.node.3))
      rf.grid <- expand.grid(
        splitrule=split.rule,
        mtry=mtry,
        min.node.size=min.node
      )
      
      # Save model inputs to variable one env level up
      model.inputs <<- list(
        control=control,
        control.w.probs=control.class.probs,
        k.fold=.inputs$k.fold,
        p=.inputs$p,
        alpha=alpha,
        lambda=lambda,
        glm.grid=glm.grid,
        cp=cp.seq,
        ct.grid=ct.grid,
        split.rule=split.rule,
        mtry=mtry,
        min.node=min.node,
        rf.grid=rf.grid
      )
      
      showModal(
        ui=modalDialog(
          id="modelModel",
          title="Confirm Model Parameters",
          size="l",
          h2("Details"),
          div(
            style="font-size:15px; display: flex; align-items: start;", 
            div( # First column for bolded content
              style="display: flex; flex-direction: column; align-items: flex-start;", 
              tags$p(HTML("<b>Split %:</b>"), style="margin: 0;"),
              tags$p(HTML("<b>CV Folds:</b>"), style="margin: 0;"),
              tags$p(tags$b("Baseline Model #1:"), style="margin: 0;"),
              tags$p(tags$b("Baseline Model #2:"), style="margin: 0;"),
              tags$p(tags$b("Custom Model #1:"), style="margin: 0;"),
              tags$p(tags$b("Custom Model #2:"), style="margin: 0;"),
              tags$p(tags$b("Custom Model #3:"), style="margin: 0;")
            ),
            div( # Second column for non-bolded content
              style="display: flex; flex-direction: column; align-items: 
                     flex-start; margin-left: 10px;", 
              tags$p(paste0(.inputs$p * 100, "%"), style="margin: 0;"),
              tags$p(.inputs$k.folds, style="margin: 0;"),
              tags$p("Inhomogenous Poisson Process (IPP) Model", style="margin: 0;"),
              tags$p("Maximum Entropy Model", style="margin: 0;"),
              tags$p(ifelse(sum(glm.grid) == 0, 
                            "GLM (Logistic Regression) Model",
                            "GLM (Logistic Regression) Model with Regularization"), 
                     style="margin: 0;"),
              tags$p("Classification Tree Model", style="margin: 0;"),
              tags$p("Random Forest Model", style="margin: 0;")
            )
          )
          
          ,
          h3("Training Grids"),
          div(
            id="modelModalContent",
            class="shiny-row",
            div(
              class="custom-border",
              style="margin:10px;",
              h4("GLM"),
              renderDT(datatable(glm.grid,
                                 filter='none',
                                 selection='none',
                                 rownames=F,
                                 options=list(
                                   scrollY="210px",
                                   paging=F,
                                   searching=F,
                                   orderMulti=T,
                                   info=T,
                                   lengthChange = F
                                 )
              ) %>%
                formatStyle(columns=names(glm.grid), `font-size`="18px")
              )
            ),
            div(
              class="custom-border",
              style="margin:10px;",
              h4("Classification Tree"),
              renderDT(datatable(ct.grid,
                                 filter='none',
                                 selection='none',
                                 rownames=F,
                                 options=list(
                                   scrollY="210px",
                                   paging=F,
                                   searching=F,
                                   orderMulti=T,
                                   info=T,
                                   lengthChange = F
                                 )
              ) %>%
                formatStyle(columns=names(ct.grid), `font-size`="18px")
              )
            ),
            div(
              class="custom-border",
              style="margin:10px;",
              h4("Random Forest"),
              renderDT(datatable(rf.grid,
                                 filter='none',
                                 selection='none',
                                 rownames=F,
                                 options=list(
                                   scrollY="210px",
                                   paging=F,
                                   searching=F,
                                   orderMulti=T,
                                   info=T,
                                   lengthChange = F
                                 )
              ) %>%
                formatStyle(columns=names(rf.grid), `font-size`="18px")
              )
            )
          ),
          footer=div(
            class="shiny-row",
            div(actionButton("confirm_model_inputs", "Confirm", icon("check"))),
            div(style="margin-left:10px;", 
                actionButton("cancel_model_inputs", "Cancel", icon("cancel"))
            )
          )
        )
      )
    }
  }) 
  
  # Initialize model output list
  model.outputs <- list()
  observeEvent(input$confirm_model_inputs, {
    
    removeModal()
    js$loadingPanel()
    
    # Reset model outputs
    model.outputs <<- list()
    
    # Incorporate the progress bar using withProgress
    withProgress(message = "Training models", value = 0, {
      # Increment the progress by 1/5 (0.2) after training each model since there are 5 models.
      
      # IPP Model Training
      ipp.fname <- paste0("ipp_p_", model.inputs$p, ".rds")
      ipp.fname <- file.path(tempdir(), ipp.fname)
      ipp.model.output <- tryCatch(
        {train.model(
          df=model.vars()$train.test.data$presence.train,
          rasters=rasters,
          full.df=df,
          model.type="ipp",
          file.loc=ipp.fname
        )},
        error = function(e) {
          showNotification(paste0("IPP Model Fit Error: ", e$message), type = "error")
          return(NULL)
        })
      incProgress(0.2, detail = "Finished training IPP model. Starting MaxEnt...")
      
      # MaxEnt Model Training
      maxent.fname <- paste0("maxent_p_", model.inputs$p, ".rds")
      maxent.fname <- file.path(tempdir(), maxent.fname)
      maxent.model.output <- tryCatch(
        {train.model(
          df=model.vars()$train.test.data$train,
          model.type="maxent",
          file.loc=maxent.fname
        )},
        error = function(e) {
          showNotification(paste0("MaxEnt Model Fit Error: ", e$message), type = "error")
          return(NULL)
        }) 
      incProgress(0.2, detail = "Finished training MaxEnt model. Starting GLM model..")
      
      # Logistic Regression Model Training
      glm.fname <- paste0("glm_p", model.inputs$p, "_kfold_", 
                          model.inputs$k.fold, "_alpha_", 
                          paste0(model.inputs$alpha, collapse=""), "_lambda_", 
                          paste0(model.inputs$lambda, collapse=""), ".rds")
      glm.fname <- file.path(tempdir(), glm.fname)
      glm.model.output <- tryCatch(
        {train.model(
          df=model.vars()$train.test.data$train.dummies,
          train.control=model.inputs$control,
          train.grid=model.inputs$glm.grid,
          model.type="glm",
          file.loc=glm.fname
        )},
        error = function(e) {
          showNotification(paste0("GLM Model Fit Error: ", e$message), type = "error")
          return(NULL)
        })
      incProgress(
        0.2, 
        detail = "Finished training GLM (Logistic Regression) model. Starting tree model...")
      
      # Classification Tree Model Training
      ct.fname <- paste0("ct_p", model.inputs$p, "_kfold_", 
                         model.inputs$k.fold, "_cp_", 
                         paste0(model.inputs$cp, collapse=""), ".rds")
      ct.fname <- file.path(tempdir(), ct.fname)
      ct.model.output <- tryCatch(
        {train.model(
          df=model.vars()$train.test.data$train.factor,
          train.control=model.inputs$control,
          train.grid=model.inputs$ct.grid,
          model.type="tree",
          file.loc=ct.fname
        )},
        error = function(e) {
          showNotification(paste0("Tree Model Fit Error: ", e$message), type = "error")
          return(NULL)
        })
      incProgress(0.2, detail = "Finished training tree model. Starting Random forest...")
      
      # Random Forest Model Training
      rf.fname <- paste0("rf_p", model.inputs$p, "_kfold_", 
                         model.inputs$k.fold, "_mtry_", 
                         paste0(model.inputs$mtry, collapse=""), 
                         "_splitrule_", paste0(model.inputs$split.rule, collapse=""), 
                         "_minnode_", paste0(model.inputs$min.node, collapse=""), ".rds")
      rf.fname <- file.path(tempdir(), rf.fname)
      rf.model.output <- tryCatch(
        {train.model(
          df=model.vars()$train.test.data$train.factor,
          train.control=model.inputs$control.w.probs,
          train.grid=model.inputs$rf.grid,
          model.type="rf",
          file.loc=rf.fname
        )},
        error = function(e) {
          showNotification(paste0("Random Forest Model Fit Error: ", e$message), type = "error")
          return(NULL)
        })
      incProgress(0.2, detail = "Finished training the Random Forest model. Training Complete.")
    })
   js$finishedLoadingPanel()
    
    # Save model outputs to variable one level up
    model.outputs <<- list(
      ipp=ipp.model.output$fit,
      maxent=maxent.model.output$fit,
      glm=glm.model.output$fit,
      ct=ct.model.output$fit,
      rf=rf.model.output$fit
    )
  })
  
  observeEvent(input$cancel_model_inputs, {removeModal()})
  
  
  ### Prediction & Evaluation #######################################
  
  # Initialize a list to hold the binned rasters
  raster.img.names <- names(rasters)[!(names(rasters) %in% c("lat", "lon"))]
  raster.imgs <- map(raster.img.names, ~as.im(rasters[[.x]]))
  names(raster.imgs) <- raster.img.names
  
  observeEvent(c(input$modelTabs, input$predictionCutoff), {
    if (input$modelTabs == "Prediction & Model Evaluation") {
      # TODO: Make sure all models have been trained
      
      # Predictions 
      
      js$loadingPanel()
      
      # Incorporate the progress bar using withProgress
      withProgress(message = "Predicting/Evaluating Models", value = 0, {
        # IPP 
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
        # on the cumulative distribution function of the Poisson distribution.
        prob.at.least.1 <- calc(predicted.raster, function(x) {1 - exp(-x)})
        crs(prob.at.least.1) <- crs(rasters[[1]])
        
        # Extract the predicted probabilities for the test points
        ipp.yhat <- raster::extract(prob.at.least.1, 
                                    model.vars()$train.test.data$test[, c("lon", "lat")])
        
        ipp.cm <- confusionMatrix(
          factor(ifelse(ipp.yhat >= input$predictionCutoff, 1, 0), levels=c(0, 1)), 
          factor(model.vars()$train.test.data$test$presence, levels=c(0, 1)),
          mode="everything",
          positive="1")
        
        incProgress(0.2, detail = "Finished predicting/evaluating the IPP model.")
        cat("Finished predicting/evaluating the IPP model\n")
        
        # MaxEnt
        # Make predictions on full raster
        maxent.raster <- dismo::predict(model.outputs$maxent, x=rasters)
        crs(maxent.raster) <- crs(rasters[[1]])
        
        # Make predictions on the test data
        test.x <- model.vars()$train.test.data$test %>% 
          dplyr::select(-c("presence"))
        test.y <- model.vars()$train.test.data$test$presence
        maxent.yhat <- predict(model.outputs$maxent, x=test.x)
        
        # Create a confusion matrix for the maxent model
        maxent.cm <- confusionMatrix(
          factor(ifelse(maxent.yhat >= input$predictionCutoff, 1, 0), levels=c(0, 1)), 
          factor(test.y, levels=c(0, 1)),
          mode="everything",
          positive = "1")
        
        # Make predictions on full raster
        maxent.raster <- dismo::predict(model.outputs$maxent, x=rasters)
        crs(maxent.raster) <- crs(rasters[[1]])
        
        incProgress(0.2, detail = "Finished predicting/evaluating the MaxEnt model.")
        cat("Finished predicting/evaluating the MaxEnt model\n")
        
        # GLM
        # Generate predictions on rasters (to plot probabilities)
        glm.raster <- 1 - raster::predict(object=binary.rasters, model=model.outputs$glm, type="prob")
        
        # Generate predictions test set
        glm.yhat <- predict(model.outputs$glm, 
                            newdata = model.vars()$train.test.data$test.dummies)
        
        # Compute metrics using confusion matrix
        glm.cm <- confusionMatrix(
          glm.yhat, model.vars()$train.test.data$test.dummies$presence, 
          positive="presence",
          mode="everything")
        
        incProgress(0.2, detail = "Finished predicting/evaluating the GLM model.")
        cat("Finished predicting/evaluating the GLM model\n")
        
        # Classification Tree
        # Generate predictions on rasters (to plot probabilities)
        ct.raster <- raster::predict(object=rasters, model=model.outputs$ct, type="prob",
                                     factors=list(biome=levels(df$biome)))
        
        # Generate predictions test set
        ct.yhat <- predict(model.outputs$ct, 
                           newdata = model.vars()$train.test.data$test.factor)
        
        # Compute metrics using confusion matrix
        ct.cm <- confusionMatrix(
          ct.yhat, model.vars()$train.test.data$test.factor$presence, 
          positive="presence",
          mode="everything")
        
        incProgress(0.2, detail = "Finished predicting/evaluating the tree model.")
        cat("Finished predicting/evaluating the tree model\n")
        
        # Random Forest
        # Generate predictions on rasters (to plot probabilities)
        
        rf.raster <- 1 - raster::predict(
          object=rasters, 
          model=model.outputs$rf,
          type="prob",
          factors=list(biome=levels(df$biome)))
        
        # Generate predictions test set
        rf.yhat <- predict(model.outputs$rf, 
                           newdata = model.vars()$train.test.data$test.factor)
        
        # Compute metrics using confusion matrix
        rf.cm <- confusionMatrix(
          rf.yhat, model.vars()$train.test.data$test.factor$presence , 
          positive="presence",
          mode="everything")
        
        incProgress(0.2, detail = "Finished predicting/evaluating the random forest model.")
        cat("Finished predicting/evaluating the random forest model\n")
        
      })
      js$finishedLoadingPanel()
      
    }
  })
  
}

