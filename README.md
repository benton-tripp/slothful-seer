# Slothful Seer
A Shiny app for estimating the distribution of the three-toed sloth 🦥🌳🔍

<hr>

## Overview

The *Slothful Seer* Shiny application is a tool developed to assist in understanding the distribution patterns of the three-toed sloth. It provides an interface for exploring observation data of the species in question, as well as other relevant data such as environmental factors and eco-regions. Utilizing machine learning and statistical models, this application utilizes the various data sources to predict the distribution of the species across the study area.

## Running the application locally

The app can be run from an R console (e.g., RStudio) either by cloning this repository, or running the app locally by directly connecting to Github through the `shiny::runGitHub()` function as demonstrated below.

```{r}

# This code that can be pasted into an R console to run locally without cloning the repo:
shiny::runGitHub(repo="benton-tripp/slothful-seer/")

```

Here is a list of packages needed to run the app, as well as code that can be run to ensure each of 
the required packages is installed:

- `shiny`
- `shinyjs`
- `shinydashboard`
- `shinybusy`
- `DT`
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
- `sf`
- `rgdal`
- `htmlwidgets`
- `rgeos`

```{r}

# Install required libraries
install.packages(c(
  "shiny", "shinyjs", "shinydashboard", "shinybusy", "DT", "tidyverse",
  "dismo", "maptools", "ggpubr", "rJava", "skimr", "leaflet", "leaflet.extras",
  "raster", "spatstat", "caret", "gbm", "plyr", "caTools", "rpart", "e1071",
  "ranger", "glmnet", "Matrix", "sp", "sf", "rgdal", "htmlwidgets", "rgeos"
))

```

Note that `rJava` also requires a 64-bit version of Java installed, along with the `JAVA_HOME` environment variable set. To install Java, see the [Oracle website](https://www.oracle.com/java/technologies/downloads/). After installation, either set the `JAVA_HOME` environment variable (e.g., using a Windows desktop run `setx PATH "C:\C:\Program Files\Java\jdk-VERSION;%PATH%"` from the command line, replacing "VERSION" with the correct version), or set it from your R environment using the `Sys.setenv()` function prior to loading the library in an R session. 

## Running the application using Docker

Docker can be used as an alternative to running the application from R locally. This is a good alternative if package versions and/or Java are giving you grief. The GIS packages (`rgdal`, `sp`, `sf`, `raster`, `dismo`, etc.) can be especially troublesome to install without any errors, depending on the OS and R version.

There are a couple of options of how you might run the app using Docker:

1. Pull the image from Docker Hub (easier/faster method)
2. Build the image from the Dockerfile included in this repository

### Pulling the image from Docker Hub

1. Run `docker pull bentontripp/slothful-seer-app:latest`
2. Then run `docker run --rm -p 4000:3838 bentontripp/slothful-seer-app:latest` to start a container
3. Open up a browser, navigate to [http:/127.0.0.1:4000](http:/127.0.0.1:4000)
4. To stop, run `docker stop [CONTAINER_ID]`

### Building the image from the Dockerfile

1. Download the Dockerfile (or clone the full app), and navigate to the directory with the file
2. Run `docker build -t slothful-seer-app .` to create an image (this can take 5-10 minutes to complete)
3. Then run `docker run --rm -p 4000:3838 slothful-seer-app` to start a container
4. Open up a browser, navigate to [http:/127.0.0.1:4000](http:/127.0.0.1:4000)
5. To stop, run `docker stop [CONTAINER_ID]`

