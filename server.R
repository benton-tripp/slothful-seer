# Options
options("rgdal_show_exportToProj4_warnings"="none")

cat("Loading libraries...\n")
# Load libraries
purrr::walk(
  c("shiny", "dismo", "sp", "maptools", "tidyverse", "ggpubr", "shinyjs", "skimr",
    "leaflet", "leaflet.extras", "raster", "spatstat", "caret", "DT", "rJava",
    "shinyWidgets"), 
  function(.x) {
    cat("Loading the", .x, "library...\n")
    suppressWarnings(suppressPackageStartupMessages(library(.x, character.only=T)))
})

# Set seed for reproducibility
set.seed(19)


cat("Sourcing modules...\n")
# Source utility functions (e.g., get_data.R loads the data)
.utils <- file.path("utils", list.files("utils"))
walk(.utils[.utils != "utils/modeling_info_sidebar.R"], function(.x) {
  cat("Sourcing", .x, "...\n")
  source(.x)
}) %>% suppressWarnings()

server <- function(input, output, session) {
  cat("Initializing Server...\n")
  ### Data Overview ###############################
  
  # Update select input
  updatePickerInput(session,  "select_columns", 
                    choices=sort(names(df)[!(names(df) %in% c("presence", "lat", "lon"))]),
                    selected=sort(names(df)[!(names(df) %in% c("presence", "lat", "lon"))]))
  # Data
  observeEvent(c(input$sidebarMenu, input$select_data_filter, input$select_columns), {
    if (input$sidebarMenu == "dataPanel") { 
      # Filter by presence
      if (input$select_data_filter == "Presence") {
        data.selection <- presence.df
      } else if (input$select_data_filter == "Absence") {
        data.selection <- pseudo.absence.df
      } else {
        data.selection <- df
      }
      
      # Filter columns; Reorder columns with "presence" first and the remaining columns sorted
      data.selection <- data.selection %>%
          select(all_of(c("presence", "lon", "lat", input$select_columns)))
      
      # Render/format data table
      output$all_data_selection <- renderDT({
        dt <- datatable(
          data.selection %>% mutate(lat=round(lat,3), lon=round(lon,3)),
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
          formatStyle(columns=names(data.selection), `font-size`="17px")
        if (length(input$select_columns) > 0 & !is.null(input$select_columns)) {
          if (!all(input$select_columns == "biome")) {
            dt <- dt %>% 
              formatSignif(columns=input$select_columns[input$select_columns != "biome"], 
                           digits=6)
          }
        }
        dt
      })
      
      #  Download data
      output$downloadData <- downloadHandler(
        filename = function() {
          paste0("bradypus_presence_only.csv")
        },
        content = function(file) write.csv(data.selection, file, row.names = F),
        contentType = 'text/csv')
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
            points(df$lon, df$lat, col = colors[factor(df$presence, levels=c(0, 1))],
                   pch = 20, cex = 0.4) 
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
  updateSelectInput(session=session,
                    inputId="continuous_var", 
                    choices=c(names(df)[!(names(df) %in% c("presence", "biome"))], "All"))
  
  observeEvent(c(input$edaTabs, input$cont_plot_type, input$select_data_filter_cont), {
    if (input$edaTabs == "Continuous Data") {
      if (input$select_data_filter_cont != "All") {
        cont.data <- df %>%
          filter(presence == ifelse(input$select_data_filter_cont == "Presence", 1, 0))
      } else cont.data <- df
      
      # Table
      if (input$cont_plot_type != "Table") {
        output$cont_plot <- renderPlot(cont.eda(cont.data, input$continuous_var, 
                                                input$cont_plot_type), width=800)
        output$cont_table <- NULL
      } else {
        output$cont_plot <- NULL
        summary.table <- cont.eda(cont.data, "All", "Table")
        cont.datatable <- format.datatable(summary.table, signif=4, searching=F, 
                                           font.size="17px", page.length=nrow(summary.table))
        output$cont_table <- renderDT(cont.datatable)
      }
      # Plot
      
    }
  })
  
  # Categorical data
  observeEvent(c(input$edaTabs, input$cat_plot_type, input$select_data_filter_cat), {
    if (input$edaTabs == "Categorical Data") {
      if (input$select_data_filter_cat != "All") {
        cat.data <- df %>%
          filter(presence == ifelse(input$select_data_filter_cat == "Presence", 1, 0))
      } else cat.data <- df
      
      summary.data <- cat.eda(cat.data, "Table") %>% arrange(-n)
      cat.datatable <- format.datatable(summary.data, page.length=nrow(summary.data), 
                                        font.size="19px", searching=F)
      # Table
      output$cat_table <- renderDT(cat.datatable)
      # Plot
      output$cat_plot <- renderPlot(cat.eda(cat.data, input$cat_plot_type), width=500)
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
  })
  
  # Save each of the models to a reactive variable
  model.vars <- reactive({
    if (input$modelTabs == "Model Fitting") {
      required_inputs <- c(input$split_perc, input$k_folds, input$glm_alpha_1, 
                           input$glm_lambda_1, input$start_cp, input$range_cp, 
                           input$step_cp, input$mtry_1, input$min_node_1)
      # Check for missing or non-numeric values
      if (any(purrr::map_lgl(required_inputs, 
                             ~(is.null(.x) | is.na(.x) | !is.numeric(.x)))) | is.null(input$split_rule)) {
        showNotification(tags$span("One or more of the required values is missing or invalid"), 
                         type="error")
        return(NULL)
      } 
      # Check for valid ranges/values
      else if (input$split_perc <= 0 | input$split_perc > 1 | 
               input$k_folds <= 0 | !is.integer(input$k_folds) |
               any(purrr::map_lgl(c(input$glm_alpha_1, input$glm_alpha_2, input$glm_alpha_3)[
                 !is.na(c(input$glm_alpha_1, input$glm_alpha_2, input$glm_alpha_3))], 
                                  ~(.x < 0 | .x > 1))) |
               any(purrr::map_lgl(c(input$glm_lambda_1, input$glm_lambda_2, input$glm_lambda_3)[
                 !is.na(c(input$glm_lambda_1, input$glm_lambda_2, input$glm_lambda_3))], 
                                  ~.x < 0)) |
               input$start_cp < 0 | input$step_cp <= 0 | input$range_cp <= 0 |
               any(purrr::map_lgl(c(input$mtry_1, input$mtry_2, input$mtry_3)[
                 !is.na(c(input$mtry_1, input$mtry_2, input$mtry_3))], 
                                  ~(.x <= 0 | !is.integer(.x)))) |
               any(purrr::map_lgl(c(input$min_node_1, input$min_node_2, input$min_node_3)[
                 !is.na(c(input$min_node_1, input$min_node_2, input$min_node_3))], 
                                  ~(.x <= 0 | !is.integer(.x))))) {
        showNotification(tags$span("One or more values is out of the expected range"), 
                         type="error")
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
          ),
          h4("Training Grids"),
          hr(style="margin:0; padding:0;"),
          div(
            id="modelModalContent",
            class="shiny-row",
            div(
              class="custom-border",
              style="margin:8px;",
              h5(tags$b("GLM")),
              renderDT(datatable(glm.grid,
                                 filter='none',
                                 selection='none',
                                 rownames=F,
                                 options=list(
                                   scrollY="170px",
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
              style="margin:8px;",
              h5(tags$b("Classification Tree")),
              renderDT(datatable(ct.grid,
                                 filter='none',
                                 selection='none',
                                 rownames=F,
                                 options=list(
                                   scrollY="170px",
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
              style="margin:8px;",
              h5(tags$b("Random Forest")),
              renderDT(datatable(rf.grid,
                                 filter='none',
                                 selection='none',
                                 rownames=F,
                                 options=list(
                                   scrollY="170px",
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
  
  # Initialize prediction lists
  pred.lis <- list()
  ipp.raster <- NULL
  
  observeEvent(c(input$modelTabs, input$predictionCutoff), {
    if (input$modelTabs == "Model Evaluation") {
      # Reset prediction list
      pred.lis <<- list()
      ipp.raster <<- NULL
      
      # Make sure all models have been trained
      if (is.empty(model.outputs) | any(is.null(model.outputs))) {
        runjs("$('#modelOutputs').css('border', '0 solid #888')")
        incompleted.models.modal()
      } else {
          
        # Predictions 
        js$loadingPanel()
        runjs("$('#modelOutputs').css('border', '1px solid #888')")
        
        # Incorporate the progress bar using withProgress
        withProgress(message = "Predicting/Evaluating Models", value = 0, {
          # IPP 
          ipp <- predict.ipp(model.outputs, raster.imgs, rasters, 
                             model.vars(), input$predictionCutoff)
          ipp.raster <<- ipp$raster
          
          incProgress(0.2, detail = "Finished predicting/evaluating the IPP model.")
          cat("Finished predicting/evaluating the IPP model\n")
          
          # MaxEnt
          maxent <- predict.evaluate.maxent(model.outputs, rasters, 
                                            model.vars(), input$predictionCutoff)
          
          incProgress(0.2, detail = "Finished predicting/evaluating the MaxEnt model.")
          cat("Finished predicting/evaluating the MaxEnt model\n")
          
          # GLM
          glm <- predict.evaluate.glm(model.outputs, binary.rasters,
                                      model.vars(), input$predictionCutoff)
          
          incProgress(0.2, detail = "Finished predicting/evaluating the GLM model.")
          cat("Finished predicting/evaluating the GLM model\n")
          
          # Classification Tree
          # Generate predictions on rasters (to plot probabilities)
          tree <- predict.evaluate.tree(model.outputs, rasters, 
                                        model.vars(), input$predictionCutoff)
          
          incProgress(0.2, detail = "Finished predicting/evaluating the tree model.")
          cat("Finished predicting/evaluating the tree model\n")
          
          # Random Forest
          rf <- predict.evaluate.rf(model.outputs, rasters, 
                                    model.vars(), input$predictionCutoff)
          
          incProgress(0.2, detail = "Finished predicting/evaluating the random forest model.")
          cat("Finished predicting/evaluating the random forest model\n")
          
        })
        runjs("Shiny.setInputValue('render_model_results', Math.random());")
        
        # Save to list
        pred.lis <<- list(ipp=ipp, maxent=maxent, glm=glm, tree=tree, rf=rf)
      }
    }
  })
  
  observeEvent(c(input$render_model_results, input$predictionCutoff), {
    if(!is.empty(pred.lis) & input$modelTabs == "Model Evaluation") {
      # Update visualizations on Prediction/Evaluation page

      model.names <- c("IPP", "MaxEnt", "GLM", "Tree", "Random Forest")
      
      model.metrics <- purrr::map2(.x=pred.lis, .y=model.names, function(.x, .y) {
        out <- .x$cm$byClass %>% 
          as.data.frame() %>% 
          rownames_to_column("Metric") 
        names(out) <- c("Metric", .y)
        out
      }) %>% Reduce(function(df1, df2) left_join(df1, df2, by="Metric"), .)
      
      output$metric_table <- renderDT({
        datatable(
          model.metrics,
          rownames=F,
          selection='none',
          options=list(
            paging=F,
            scrollX=T,
            scrollY=F,
            searching=F,
            orderMulti=T,
            info=F,
            lengthChange=F,
            pageLength=11
          )
        ) %>%
          formatStyle(columns=names(model.metrics), `font-size`="17px") %>%
          formatSignif(columns=2:6, digits=4)
      })
      
      # Confusion Matrices
      output$confusion_matrix <- renderUI({
        div(
          div(renderDT(show.cms(pred.lis, model.names))),
          # Legend
          tags$div(
            style="padding: 5px;",
            tags$p(paste0("Green: Correct prediction. ",
                          "Darker shade indicates higher recall (TP) or specificity (TN).")),
            tags$p(paste0("Red: Incorrect prediction. ",
                   "Darker shade indicates higher FP or FN rates."))
        )
      )
      })
      
      # Render output rasters
      output$raster_estimate <- renderPlot({
        plot.preds(pred.lis, "Estimated Probability Raster", model.names)
      }, width=1000, height=500)
      
      output$pred_map <- renderPlot({
        plot.preds(pred.lis, "Predicted vs. Actual Map", model.names)
      }, width=1000, height=500)
      
      output$prob_density <- renderPlot({
        plot.preds(pred.lis, "Probability Density Plot", model.names)
      }, width=1000, height=500)
      
      output$pred_bar <- renderPlot({
        plot.preds(pred.lis, "Bar Plot", model.names)
      }, width=1000, height=500)
    }
    js$finishedLoadingPanel()
  })
  
  # Show/hide the visualization options on the model evaluation tab
  observeEvent(input$toggleRadioBtn, {
    runjs("
      if ($('#radioSection').css('max-width') === '175px') {
       $('#radioSection').css('max-width', 0);
       $('#radioSection').css('visibility', 'hidden');
       $('#toggleRadioBtn').text('+');
      } else {
        $('#radioSection').css('max-width', '175px');
        $('#radioSection').css('visibility', 'visible');
        $('#toggleRadioBtn').text('-');  
      }
    ")
  })
  
  # Prediction tab
  observe({
    if (input$modelTabs == "Prediction") {
      # Make sure all models have been trained
      if (is.empty(model.outputs) | any(is.null(model.outputs))) {
        output$predictionMap <- NULL
        incompleted.models.modal()
      } else {
        # Render interactive plot through which to generate predictions by point
        output$predictionMap <- renderLeaflet({
          map.data(df, 
                   points=F, 
                   max.bounds=c(min(df$lon), min(df$lat), max(df$lon), max(df$lat)),
                   hover=T,
                   raster.crs=crs(rasters[[1]])) %>% 
            addPolygons(data=rgeos::gUnaryUnion(raster::crop(south.america, raster::extent(rasters[[1]]))),
                              color=NA, fill=T, fillOpacity=0.15, fillColor="red") %>% 
            addLegend(position = "topright", colors = "red", 
                            labels = "Observation Area", opacity = 0.15)
        })
        
        # Render hover text (lon/lat) output
        output$hoverText <- renderText({
          if(is.null(input$hover_coordinates)) {
            "Lat: None\nLng: None"
          } else {
            paste0("Lat: ", input$hover_coordinates[1], 
                   "\nLng: ", input$hover_coordinates[2])
          }
        })
        
      }
    }
  })
  
  # When the user clicks on the map:
  observeEvent(input$click_coordinates, {
    if (input$modelTabs == "Prediction") {
      # Make sure all models have been trained
      if (is.empty(model.outputs) | any(is.null(model.outputs))) {
        output$predictionMap <- NULL
        incompleted.models.modal()
      } else {
        # Get point as dataframe
        .point <- data.frame(lon=input$click_coordinates[2], lat=input$click_coordinates[1])
        .method <- ifelse(input$cell_select_method == "Simple", "simple", "bilinear")
        # Get values at point from rasters
        raster.vals <- as.data.frame(raster::extract(rasters, .point, method=.method)) %>%
          mutate(biome = factor(biome, levels=levels(df$biome)))
        binary.raster.vals <- raster::extract(binary.rasters, .point, method=.method)
        # Since IPP is picky, make sure you have the raster:
        if (is.null(ipp.raster)) {
          ipp.raster <<- predict.ipp(model.outputs, raster.imgs, rasters,
                                     model.vars(), input$predictionCutoff)$raster
        }
        if (any(is.na(raster.vals))) {
          output$predictionOutput <- NULL
          showNotification(tags$span("Please select a point within the observation area"), 
                           type="error")
        } else {
          # Get prediction of point based on model type
          if (input$select_pred_model == "IPP") {
            pred <- predict.ipp.point(ipp.raster, .point)
          } else if (input$select_pred_model == "MaxEnt") {
            pred <- predict.maxent.point(model.outputs$maxent, raster.vals)
          } else if (input$select_pred_model == "GLM") {
            pred <- predict.point(model.outputs$glm, binary.raster.vals)$presence
          } else if (input$select_pred_model == "Classification Tree") {
            pred <- predict.point(model.outputs$ct, raster.vals)$presence
          } else if (input$select_pred_model == "Random Forest") {
            pred <- predict.point(model.outputs$rf, raster.vals)$presence
          }
          cat("Model:", input$select_pred_model, "=", pred, "at {", .point$lon, .point$lat, "}\n")
          # Render/format output table
          output$predictionOutput <- renderDT({
            pred.df <- cbind(.point, select(raster.vals, -lat, -lon)) %>% 
              t() %>% 
              as.data.frame() %>%
              rownames_to_column("Variable") %>%
              rename(Value = "V1") %>%
              rbind(data.frame(
                Variable=c("prediction", "probability"),
                Value=c(ifelse(pred >= input$predictionCutoff_2, "presence", "no.presence"), 
                        round(pred, 5))
              ), .)
            dt <- datatable(data=pred.df, 
                            filter='none',
                            selection='none',
                            rownames=F,
                            options=list(
                              paging=F,
                              searching=F,
                              orderMulti=F,
                              info=F,
                              lengthChange = F
                            )
            ) %>%
              formatStyle(columns=names(pred.df), `font-size`="15px")
          })
        }
         
      }
    }
  })
  
  # After everything is loaded, send a message to hide the loading screen
  session$sendCustomMessage(type = 'hideLoadingScreen', message = 'hide')
  cat("Application load successful!\n")
}

