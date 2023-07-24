# Slothful Seer
A Shiny app for estimating the distribution of the three-toed sloth 🦥🌳🔍

<hr>

## Overview

The *Slothful Seer* Shiny application is a tool developed to assist in understanding the distribution patterns of the three-toed sloth. It provides an interface for exploring observation data of the species in question, as well as other relevant data such as environmental factors and eco-regions. Utilizing machine learning and statistical models, this application utilizes the various data sources to predict the distribution of the species across the study area.

## Running the application

The app can be run from an R console (e.g., RStudio) either by cloning this repository, or running the app locally by directly connecting to Github through the `shiny::runGitHub()` function as demonstrated below.

```{r}

# This code that can be pasted into an R console and run locally
# without cloning this repository:
shiny::runGitHub(repo="benton-tripp/slothful-seer/")

```

Here is a list of packages needed to run the app, as well as code that can be run to ensure each of 
the required packages is installed:

- `shiny`
- `shinyjs`
- `shinydashboard`
- `tidyverse`
- `dismo`
- `maptools`
- `ggpubr`
- `rJava`
- `skimr`
- `leaflet`
- `leaflet.extras`
- `raster`
- `spatstat`
- `caret`
- `gbm`
- `plyr`
- `caTools`
- `rpart`
- `e1071`
- `ranger`
- `glmnet`
- `Matrix`
- `sp`

```{r}

# Install required libraries

# Install required libraries
install.packages(c(
  "shiny",
  "shinyjs",
  "shinydashboard",
  "tidyverse",
  "dismo",
  "maptools",
  "ggpubr",
  "rJava",
  "skimr",
  "leaflet",
  "leaflet.extras",
  "raster",
  "spatstat",
  "caret",
  "gbm",
  "plyr",
  "caTools",
  "rpart",
  "e1071",
  "ranger",
  "glmnet",
  "Matrix",
  "sp"
))


```


