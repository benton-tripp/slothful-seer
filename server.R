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
        output$cont_plot <- renderPlot(cont.eda(df, input$continuous_var, input$cont_plot_type), width=800)
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
  
  ### Modeling #######################################
  
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
      dummy.data <- convert.biomes.to.dummy(prepared.data$train, prepared.data$test)
      
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
  
  observeEvent(model.vars(), {
    if (!is.null(model.vars())) {
      # This observer will run when `model.vars` gets updated and is not NULL
      .inputs <- model.vars()$model.variables
      
      # Define training cv control
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
      ct.grid <- data.frame(cp=clean.inputs(.inputs$cp))
      
      # Random Forest Grid
      split.rule <- clean.inputs(.inputs$split.rule)
      mtry <- clean.inputs(c(.inputs$mtry.1, .inputs$mtry.2, .inputs$mtry.3))
      min.node <- clean.inputs(c(.inputs$min.node.1, .inputs$min.node.2, .inputs$min.node.3))
      rf.grid <- expand.grid(
        splitrule=split.rule,
        mtry=mtry,
        min.node.size=min.node
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
              style="display: flex; flex-direction: column; align-items: flex-start; margin-left: 10px;", 
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
  
  observeEvent(input$confirm_model_inputs, {
    removeModal()
    showNotification(HTML("<b>Modeling Stuff...</b>"))
  })
  observeEvent(input$cancel_model_inputs, {removeModal()})
  
  
}

