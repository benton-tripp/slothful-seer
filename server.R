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
  
  observeEvent(
    eventExpr=input$apply_model_updates, # Only when apply is clicked
    handlerExpr={
      if (input$modelTabs == "Model Fitting") {
        train.index <- stratified.split.idx(df, p=input$split_perc)
        df.train <- df[train.index, ] 
        df.test <- df[-train.index, ] 
      }
    },
    ignoreInit = T
  )
  
  # 
  
  # Get presence-only from train/test (no pseudo-absence data for IPP)
  # presence.df.train <- df.train %>% filter(presence == 1)
  # presence.df.test <- df.test %>% filter(presence == 1)
  # Create a dataframe suitable for the maxent model
  # maxent.df.train <- df.train[, c("lon", "lat", "presence")]
  # Transform data into the format needed for Maxent
  # train.x <- df.train %>% 
  #   dplyr::select(-c("lon", "lat", "presence")) 
  # train.y <- df.train$presence
  # test.x <- df.test %>% 
  #   dplyr::select(-c("lon", "lat", "presence"))
  # test.y <- df.test$presence
  # Convert biomes to binary dummy variables (if needed)
  # dummy.biome.train <- model.matrix(~biome-1, data=df.train)
  # dummy.biome.test <- model.matrix(~biome-1, data=df.test)
  # df.train.factor <- df.train %>%
  #   mutate(presence = factor(presence, levels=c(0, 1)))
  # df.test.factor <- df.test %>%
  #  mutate(presence = factor(presence, levels=c(0, 1)))
  # df.train.dummies <- cbind(dplyr::select(df.train, -biome), dummy.biome.train)
  # df.test.dummies <- cbind(dplyr::select(df.test, -biome), dummy.biome.test)
  
}

