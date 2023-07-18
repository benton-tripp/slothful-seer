# Load libraries
purrr::walk(
  c("shiny", "dismo", "sp", "maptools", "tidyverse", "ggpubr", "skimr", "knitr", 
    "dismo", "leaflet", "raster", "spatstat", "maxnet", "caret", "DT"), 
  ~suppressWarnings(suppressPackageStartupMessages(library(.x, character.only=T)))
)

# Set seed for reproducibility
set.seed(19)

# Source utility functions (e.g., get_data.R loads the data)
.utils <- file.path("utils", list.files("utils"))
walk(.utils, ~source(.x)) %>% suppressWarnings()


function(input, output, session) {
  
  ### Exploratory Analysis ########################
  
  # Map
  # map.data(df, rasters, .raster=9, title="Three-Toed-Sloths | Biomes")
  
  # Continuous Data
  
  # Categorical data
  cat.data <- cat.eda(df, "table") %>% arrange(-n)
  cat.datatable <- datatable(
    cat.data,
    filter='none',
    rownames=F,
    options=list(
      dom='t',
      paging=T,
      searching=T,
      orderMulti=T,
      info=F
    )
  ) %>%
    formatStyle(columns=names(cat.data), `font-size`="20px")
  
  observeEvent(c(input$edaTabs, input$cat_plot_type), {
    if (input$edaTabs == "Categorical Data") {
      # Table
      output$cat_table <- renderDT(cat.datatable)
      # Plot
      output$cat_plot <- renderPlot(cat.eda(df, input$cat_plot_type), width=500)
    }
  })
  
  ### Create the training and test datasets #######
  
  # train.index <- stratified.split.idx(df)
  # df.train <- df[train.index, ] 
  # df.test <- df[-train.index, ] 
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

