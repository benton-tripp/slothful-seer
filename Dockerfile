FROM rocker/shiny:latest

# Install Java and Git
RUN apt-get update && apt-get install -y \
    openjdk-11-jdk \
    git \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/

# Set the JAVA_HOME environment variable
ENV JAVA_HOME /usr/lib/jvm/java-11-openjdk-amd64

# Clone your Shiny app from GitHub
RUN git clone https://github.com/benton-tripp/slothful-seer.git /srv/shiny-server/

# Install required R packages
RUN install2.r \
    shinyjs \
    shinydashboard \
    shinybusy \
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
    sf

# Make the Shiny app available at port 3838
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/')"]

# docker build -t slothful-seer-app .
# docker run --rm -p 3838:3838 slothful-seer-app
# docker stop [CONTAINER_ID]

