FROM rocker/shiny:latest

# Install Java, GDAL 3.0, PROJ 6, and Units
RUN apt-get update && apt-get install -y \
    openjdk-11-jdk \
    libproj-dev \
    libgdal-dev \
    libudunits2-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/


# Set the JAVA_HOME environment variable
ENV JAVA_HOME /usr/lib/jvm/java-11-openjdk-amd64

# Install required R packages
RUN install2.r --error --deps TRUE \
    shinyjs \
    shinydashboard \
    shinybusy \
    DT \
    tidyverse \
    dismo \
    maptools \
    ggpubr \
    rJava \
    skimr \
    leaflet \
    leaflet.extras \
    raster \
    spatstat \
    caret \
    gbm \
    plyr \
    caTools \
    rpart \
    e1071 \
    ranger \
    glmnet \
    Matrix \
    sp \
    sf \
    rgdal \
    htmlwidgets \
    rgeos \
    shinyWidgets

# Make the Shiny app available at port 3838
EXPOSE 3838

CMD ["R", "-e", "shiny::runGitHub('benton-tripp/slothful-seer', host = '0.0.0.0', port = 3838)"]



# docker build -t slothful-seer-app .
# docker run --rm -p 4000:3838 slothful-seer-app
# Open up a browser, navigate to http:/127.0.0.1:4000

# HELPFUL COMMANDS

# docker ps
# docker stop [CONTAINER_ID]

# docker rmi slothful-seer-app -f

# docker login
# docker tag slothful-seer-app:latest bentontripp/slothful-seer-app:latest
# docker push bentontripp/slothful-seer-app:latest
# docker pull bentontripp/slothful-seer-app:latest


