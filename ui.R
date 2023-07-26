# load libraries
suppressWarnings(suppressPackageStartupMessages(library(shiny)))
suppressWarnings(suppressPackageStartupMessages(library(shinydashboard)))
suppressWarnings(suppressPackageStartupMessages(library(shinyjs)))
suppressWarnings(suppressPackageStartupMessages(library(DT)))
source("utils/modeling_info_sidebar.R")

main.sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "sidebarMenu",
    menuItem(span("About", class="bold-panel-header"), tabName = "overviewPanel", 
             icon = icon("circle-info")),
    menuItem(span("Data", class="bold-panel-header"), 
             tabName = "dataPanel", icon = icon("table")),
    menuItem(span("Data Exploration", class="bold-panel-header"), 
             tabName = "edaPanel", icon = icon("map")),
    menuItem(span("Modeling", class="bold-panel-header"), 
             tabName = "modelPanel", icon = icon("line-chart"))
  )
)

overview.page <- conditionalPanel(
  "input.sidebarMenu == 'overviewPanel'",
  id="overViewConditionalPage",
  tabItems(
    tabItem(
      tabName = "overviewPanel",
      tabsetPanel(
        id = "overviewTabs",
        tabPanel(
          "Introduction",
          div(
            style="margin:10px;",
            id="overviewIntroSection",
            h3("Introduction and Purpose of this Application"),
            div(
              div(
                id="slothImg", 
                div(
                  style="display:flex; flex-direction:column;",
                  tags$img(src="images/sloth-cropped.png", alt="Sloth", style="width:200px; height:300px;"),
                  span(id="slothCaption", 
                       tags$i(
                         "Image obtained from ", 
                         tags$a(href=paste0("https://res.cloudinary.com/dk-find-out/image/",
                                            "upload/q_80,w_1920,f_auto/A-dreamstime_xxl_418", 
                                            "01886_by73ed.jpg"), "Cloudinary")
                       )
                  )
                )
              ),
              div(
                HTML(
                  paste0(
                    "<p>The purpose of this application is to model ", 
                    "the species distribution of the three-toed sloth ", 
                    "using data science methods and spatial statistics. ",
                    "This type of model is commonly used in the fields of ", 
                    "biology and ecology, and is called <i>presence-only", 
                    "</i> modeling, (since the data only entails observations ",
                    "of a species presence, and not the inverse). The contents ", 
                    "of the app include:</p>")
                ),
                tags$ul(
                  tags$li(paste0(
                    "Data View: The user is able to explore the raw data in a",
                    " tabular format. The interface allows for sorting, filtering,",
                    " and downloading the data for external analyses.")
                  ),
                  tags$li("Basic exploratory analysis: In this part of the application, the user 
                       is able to gain a deeper understanding of the observation data, as well as the
                       distribution of the variables used within the modeling process."),
                  tags$li(paste0("Modeling the data: This part of the app includes several",
                                " key components:"),
                          tags$ul(
                            tags$li(
                              paste0(
                                "Splitting the data: As with most supervised learning problems,",
                                " the data is split into training/test sets in order to ",
                                "evaluate model performance on unseen data.")
                            ),
                            tags$li(
                              paste0(
                                "Cross Validation and Model Training: the models are trained ",
                                "using a grid of user-selected hyperparameters. Cross validation",
                                " is used to iteratively test the different parameter ",
                                "combinations as well as avoid overfitting the model. The",
                                " best combination of parameters is selected as the final ",
                                "model based on its performance."
                              )
                            ),
                            tags$li(
                              paste0(
                                "Baseline models: Inhomogeneous Poisson Process (IPP) model ",
                                "and Maximum Entropy (MaxEnt) models serve as \"baseline\" ",
                                " models, given their frequent usage in presence-only ",
                                "prediction problems.")
                              ),
                            tags$li(
                              paste0(
                                "Machine learning models: Other models used less frequently ",
                                "in presence-only prediction problems are used to model the ",
                                " data, including a generalized linear model with optional ",
                                "regularization, tree-based classification, and random forest."
                              )
                            ),
                            tags$li(
                              paste0("Model evaluation: Each of the models is compared and",
                                     "evaluated through various metrics and visualizations.")
                              
                            )
                          )
                  )
                  )
                )
            )
          )
        ),
        tabPanel(
          "Data Overview",
          div(
            h3("Data Overview"),
            id="dataOverviewSection",
            HTML(paste0("<p>The <code>dismo</code> package provides certain built-in", 
            " datasets that can be used for demonstration purposes. <i>Note that the ", 
            "built-in datasets might have been scaled or modified. E.g., the temperature", 
            " raster data used in this analysis appears to have been scaled since the", 
            " min/max values range from about -200 to 400.</i></p>")),
            HTML("<p>The data from the <code>dismo</code> package used in this application 
                 includes:</p>"),
            div(
              tags$ul(
                tags$li(
                  HTML("<p>Bradypus (Three-Toed Sloth) observations in South America: This 
                      data was obtained from the Global Biodiversity Information Facility 
                      (<a href='https://www.gbif.org/'>GBIF</a>) via the following function 
                      in R: <br><code>dismo::bradypus::gbif(\"Bradypus\", \"variegatus*\", sp=T)
                      </code></p>")
                ),
                tags$li(
                  p(
                    "Pseudo Absence Data: To complement the presence data for Bradypus variegatus in 
                    the study, pseudo absence points were generated. These points were spatially 
                    randomized across the same extent as the presence data, ensuring a non-overlapping
                    distribution with the presence observations.  It's crucial to note that these points 
                    do not represent real-world observations of the species' absence but rather serve as a
                    hypothetical baseline to aid in modeling efforts. Consequently, any interpretation
                    should be made with this context in mind."
                  )
                ),
                tags$li(
                  HTML("<p>The following bioclimate data is also used from the <code>dismo</code> 
                         package: </p>"),
                  tags$ul(
                    tags$li(
                      HTML("<p>Mean annual temperature (<i>bio1</i>)</p>")
                    ),
                    tags$li(
                      HTML("<p>Max temperature of warmest month (<i>bio5</i>)</p>")
                    ),
                    tags$li(
                      HTML("<p>Min temperature of coldest month (<i>bio6</i>)</p>")
                    ),
                    tags$li(
                      HTML("<p>Temperature annual range (<i>bio7</i>)</p>")
                    ),
                    tags$li(
                      HTML("<p>Mean temperature of the wettest quarter (<i>bio8</i>)</p>")
                    ),
                    tags$li(
                      HTML("<p>Total (annual) precipitation (<i>bio12</i>)</p>")
                    ),
                    tags$li(
                      HTML("<p>Precipitation of wettest quarter (<i>bio16</i>)</p>")
                    ),
                    tags$li(
                      HTML("<p>Precipitation of driest quarter (<i>bio17</i>)</p>")
                    ),
                    tags$li(
                      HTML(paste0("<p>Terrestrial Eco-regions (Biomes) of the world,",
                         " by the World Wildlife Fund (<i>biome</i>). See ",
                         "<a href='https://www.worldwildlife.org/publications",
                         "/terrestrial-ecoregions-of-the-world'>this publication</a>",
                         " by the WWF for more details.</p>"))
                    )
                  )
                )
              )
            ),
            br(),
            h3("Study Area"),
            div(
              id="regionMap",
              style="margin:25px;",
              tags$img(src="images/study_area.png", alt="Area Map", 
                       style="width:125%; height:125%;"),
            ),
          )  
        )
      )
    )
  )
)

data.page <- conditionalPanel(
  condition="input.sidebarMenu == 'dataPanel'",
  div(
    id="edaDataSection",
    div(
      class="shiny-row",
      radioButtons("select_data_filter", label="Select Data", inline=T, 
                   choices=c("Presence", "Absence", "All"), selected="All"),
      div(
        style="margin-left:15px",
        downloadButton("downloadData", 
                     "Download Selection", 
                     icon=icon("download", lib="font-awesome"))
      )
    ),
    DTOutput("all_data_selection")
  )
)

eda.page <- conditionalPanel(
  condition="input.sidebarMenu == 'edaPanel'",
  id="edaConditionalPage",
  tabItems(
    tabItem(
      tabName = "edaPanel",
      tabsetPanel(
        id = "edaTabs",
        tabPanel(
          "Maps",
          div(
            id="edaMapSection",
            radioButtons("map_type", label="Select Map", 
                         choices=c("Interactive Map of Points", "Rasters"), 
                         inline=T, selected="Interactive Map of Points"),
            conditionalPanel(
              "input.map_type == 'Rasters'",
              div(
                class="shiny-row",
                selectInput("map_selection", label="Select Raster", selected="Mean Temperature", 
                            choices=c("Mean Temperature", "Annual Precipitation", 
                                      "Precipitation - Wettest Qrtr.", 
                                      "Precipitation - Driest Qrtr.",
                                      "Max Temperature", "Min Temperature",
                                      "Temperature Range", "Mean Temperature - Wettest Qrtr.",
                                      "Biomes", "Longitude", "Latitude")),
                div(
                  style="margin-left:10px;",
                  radioButtons("map_absence", label="Plot Points", 
                               choices=c("None", "Presence", "Absence", "Both"),
                               selected="None", inline=T)
                )
              ),
              plotOutput("raster_plot")
            ),
            conditionalPanel(
              "input.map_type == 'Interactive Map of Points'",
              uiOutput("map")
            )
          )
        ),
        tabPanel(
          "Continuous Data",
          div(
            id="edaContSection",
            div(
              radioButtons("cont_plot_type", label="Select Visualization Type", inline=T, 
                           choices=c( "Table", "Histogram", "Density", "Combined Plot"), 
                           selected="Table"),
              conditionalPanel(
                style="min-width:600px; margin-top:15px; margin-left:15px;",
                "input.cont_plot_type == 'Table'",
                div(style="margin-bottom:6px;", DTOutput("cont_table"))
              ),
              conditionalPanel(
                style="min-width:780px; margin-top:15px; margin-left:15px;",
                "input.cont_plot_type != 'Table'",
                selectInput("continuous_var", "Select Variable", choices=NULL),
                plotOutput("cont_plot")
              )
            )
          )
        ),
        tabPanel(
          "Categorical Data",
          div(
            id="edaCatSection",
            div(
              class="shiny-row",
              style="overflow-x:scroll;",
              div(
                style="max-width:60px; min-width:60px; margin:5px;",
                DTOutput("cat_table"),
              ),
              div(
                style="min-width:500px; margin-top:15px; margin-left:130px;",
                radioButtons("cat_plot_type", label="Select Plot Type", inline=T, 
                             choices=c("Bar", "Pie"), selected="Bar"),
                plotOutput("cat_plot")
              )
            )
          )
        ),
        tabPanel(
          "Presence/Absence",
          div(
            id="edaTargetSection",
            div(
              class="shiny-row",
              style="overflow-x:scroll;",
              div(
                style="max-width:60px; min-width:60px; margin:5px;",
                DTOutput("pres_table"),
              ),
              div(
                style="min-width:500px; margin-top:15px; margin-left:150px;",
                radioButtons("pres_plot_type", label="Select Plot Type", inline=T, 
                             choices=c("Bar", "Pie"), selected="Bar"),
                plotOutput("pres_plot")
              )
            )
          )
        )
      )
    )
  )
)

model.page <- conditionalPanel(
  condition="input.sidebarMenu == 'modelPanel'",
  
  id="modelConditionalPage",
  tabItems(
    tabItem(
      tabName = "modelPanel",
      tabsetPanel(
        id = "modelTabs",
        tabPanel(
          "Model Info",
          div(
            class="modeling-info-section",
            div(
              id="modelingInfoText",
              conditionalPanel(
                condition="input.menuItemSelected == 'mdlOverviewSelect'",
                div(
                  id="trainTestOverviewSection",
                  modeling.info.sidebar,
                  div( 
                    class="model-overview-page",
                    h3("Modeling Overview"),
                    div(
                      h4("Splitting the Data into Train/Test Sets"),
                      p(
                        "The process of partitioning the dataset into training and testing subsets",
                        "is a crucial step in model evaluation. A stratified sampling approach is",
                        "employed to ensure that both subsets are representative of the overall data",
                        "distribution, especially in terms of spatial and categorical variables."
                      )
                    ),
                    div(
                      h4("Spatial Stratification"),
                      p(
                        "The dataset is divided into grids based on latitude and longitude values.",
                        " This spatial stratification ensures that the train/test split is",
                        " geographically representative and avoids over-concentration of ",
                        "data points in specific regions."
                      )
                    ),
                    div(
                      h4("Categorical Stratification"),
                      p(
                        "In addition to spatial stratification, the data is further stratified based on",
                        "categorical variables, specifically the different biomes (eco-regions). This ",
                        "ensures that the distribution of these categorical variables is consistent",
                        "across both training and testing subsets."
                      )
                    ),
                    div(
                      h4("Stratified Target"),
                      p(
                        "The target variable (presence) is also used to further stratify the data. ",
                        "This ensures that the train and test sets have approximately the same ",
                        "percentage of samples of each target class as the complete set. This is ", 
                        "particularly important for imbalanced datasets where one class significantly",
                        " outnumbers the other. Without stratification, there's a risk that the ", 
                        "minority class might not be represented in the train or test set."
                      )
                    ),
                    div(
                      h4("Data Partitioning"),
                      p(
                        "Once stratification is complete, the data is partitioned into training and",
                        "testing sets based on a user-specified proportion, \"Split %\"."
                      )
                    ),
                    div(
                      h3("Model Training"),
                      div( 
                        p(
                          "The model training phase is pivotal in the development",
                          "of predictive models. For the purpose of this application,",
                          "two baseline models, IPP and MaxEnt, are trained without",
                          "any user-defined parameters. These models serve as reference",
                          "points for comparison against user-defined models, given their",
                          "prominence in Species Distribution modeling methods."
                        ),
                        p(
                          "MaxEnt, short for 'Maximum Entropy', is a widely-used method",
                          "for species distribution modeling. In this application, the",
                          "MaxEnt model is implemented using the ", tags$code("dismo"), 
                          "package, which acts as a wrapper to the original MaxEnt software",
                          "written in Java by Phillips, Dudik, and Schapire. More details",
                          "about the original software can be found at the ", 
                          tags$a(href="https://biodiversityinformatics.amnh.org/open_source/maxent/", 
                                 "official MaxEnt website"), "."
                        ),
                        p(
                          "The IPP (Inhomogeneous Poisson Process) model is another method",
                          "used for presence-only predictions. The model is trained using the", 
                          tags$code("spatstat"), " package. The process involves converting",
                          "the data into a point pattern object (", tags$code("ppp"), ") and then",
                          "fitting the IPP model using environmental covariates. The covariates",
                          "are represented as raster images, and the model formula is dynamically",
                          "constructed based on the available rasters."
                        ),
                        tags$span(class="math inline", 
                                  HTML("\\( locations \\sim covariate_1 + covariate_2 + \\ldots \\)")),
                        p(
                          "This formula represents the relationship between the locations and",
                          "the environmental covariates. The IPP model is then fitted using the", 
                          tags$code("ppm"), " function, with the constructed formula and raster",
                          "images as covariates."
                        ),
                        p(
                          "Beyond the baseline models, this application also facilitates",
                          "the training of three user-defined models: GLM (Logistic Regression),",
                          "Classification Tree, and Random Forest. These models are tailored",
                          "based on user input and are designed to provide more flexibility",
                          "and specificity in predictions.",
                          "To ensure robustness and generalizability, each of these models",
                          "is trained using k-fold cross-validation. The value of ",
                          tags$span(class="math inline", HTML("\\( k \\)")),
                          ' "Cross-Validation Folds"',
                          "is user-defined, allowing for a customizable validation approach.",
                          "During the cross-validation process, the optimal combination of",
                          "hyperparameters, as provided by the user, is determined. This ensures",
                          "that the models are not only tailored to the data but also optimized",
                          "for the best predictive performance."
                        ),
                        p(
                          "Further details on each model type, their intricacies, and their",
                          "respective hyperparameters will be discussed in subsequent sections."
                        )
                      )
                    )
                  )
                )
              ),
              conditionalPanel(
                condition="input.menuItemSelected == 'mdlIPPOverviewSelect'",
                div(
                  id="ippOverviewSection",
                  modeling.info.sidebar,
                  div(
                    class="model-overview-page",
                    h3("Inhomogeneous Poisson Process (IPP)"),
                    p("An Inhomogeneous Poisson Process (IPP) model is a statistical model used ",
                      "for events that occur randomly over space or time. Unlike a regular Poisson ",
                      "process, the rate of event occurrence in an IPP can vary. IPP models are ",
                      "often used in presence-only problems to model the intensity of events ",
                      "across different locations or times. The IPP can be defined as:"
                    ),
                    p(tags$span(class="math inline", "\\(\\lambda(x) = N \\cdot g(x)\\)"), 
                      ", where ", tags$span(class="math inline", HTML("\\(N\\)")), 
                      " is the total number of events, and ", tags$span(class="math inline", 
                                                                        HTML("\\(g(x)\\)")),
                      " is the density function."),
                    tags$i("In the case of the IPP model in this ",
                           "application, the intensity ", 
                           tags$span(class="math inline", HTML("\\(\\lambda\\)")),
                           "is not a constant, but is a function of the location and covariates ",
                           tags$span(class="math inline", HTML("\\(x\\)")),
                           ". Hence the term ‘inhomogeneous’."),
                    p("By fitting an IPP to presence-only data, we estimate the underlying 
                  intensity function ", tags$span(class="math inline", 
                                                  HTML("\\(\\lambda(x)\\)")),
                      ". This informs us about how the rate of event occurrence — in this ",
                  "case, the number of three-toed sloth counts — changes across different 
                  times or locations. The fitted model predicts the count of sloths at each point.",
                  "Because the other models in this application model probabilities, an extra step is ",
                  "needed to calculate the probability at each point that the count is at least 1:"),
                    p(
                      tags$span(class="math inline", HTML("\\(P(At\\ least\\ 1) = 1 - e^{-x}\\)")),
                      ", where ", 
                      tags$span(class="math inline", HTML("\\(x\\)")), 
                      " represents the predicted values from the intensity function ",
                      tags$span(class="math inline", HTML("\\(\\lambda(x)\\)")),
                      "for each location in the raster. This is derrived from the PDF and CDF, ",
                      "given", tags$span(class="math inline", HTML("\\(\\lambda(x)\\)")), "."),
                    tags$ul(
                      tags$li(
                        tags$span(class="math inline", 
                                  "\\( PDF(x) = \\lambda e^{-\\lambda x}, \\quad x \\geq 0\\)")
                      ),
                      tags$li(
                        tags$span(class="math inline", 
                                  HTML("\\( CDF(x) = 1 - e^{-\\lambda x}, \\quad x \\geq 0\\)"))
                      )
                    ),
                    h4("IPP Assumptions"),
                    tags$ul(
                      tags$li(
                        "Independence: It is assumed that the events occur independently in space 
                    or time, meaning the occurrence of an event at one location or time does 
                    not affect the occurrence of an event at another location or time."
                      ),
                      tags$li(
                        "Inhomogeneity: It is assumed that the intensity function can vary across 
                    space or time, as opposed to a homogeneous poisson process, which assumes 
                    a constant rate of event occurrence."
                      ),
                      tags$li(
                        "Known Intensity Function: An IPP assumes that the form of the intensity 
                   function is known, although the parameters of the function need to be 
                   estimated from the data. This assumption can be violated if the true 
                   intensity function is not well-captured by the chosen form." 
                      ),
                      tags$li(
                        "Complete Spatial Coverage: An IPP assumes that the entire study area
                    has been surveyed and that presence data is available for all locations 
                    where the species is present. This assumption can be violated due to 
                    incomplete or biased sampling."
                      )
                    )
                  )
                )
              ),
              conditionalPanel(
                condition="input.menuItemSelected == 'mdlMEOverviewSelect'",
                div(
                  id="maxEntOverviewSection",
                  modeling.info.sidebar,
                  div( 
                    class="model-overview-page",
                    h3("Maximum Entropy Model (MaxEnt)"),
                    p(
                      "Maximum Entropy (MaxEnt) model is a ",
                      "method designed to predict the probability distribution that is most spread",
                      "out, or of maximum entropy, subject to known constraints. In the context of",
                      "presence-only prediction, MaxEnt is commonly used to estimate the potential",
                      "distribution of occurrences (e.g., species observations)",
                      " based on environmental constraints."
                    ),
                    div(
                      h4("Entropy Function"),
                      p(
                        "Entropy is a measure of uncertainty or randomness. The MaxEnt model aims",
                        "to maximize this entropy given the constraints. The entropy function is ",
                        "defined as:"
                      ),
                      tags$p(
                        tags$span(
                          class="math inline", 
                          HTML("\\( H(p) = -\\sum_{i} p(x_i) \\log(p(x_i)) \\)")
                          ),", where ", tags$span(class="math inline", HTML("\\( p(x_i) \\)")), 
                        " is the probability of event ", 
                        tags$span(class="math inline", HTML("\\( x_i \\)")), "."
                      )
                    ),
                    div(
                      h4("Constraints"),
                      p(
                        "Constraints in MaxEnt are derived from the input data, ensuring that the",
                        "predicted distribution matches the empirical distribution in terms of",
                        "expected feature values. These constraints help guide the model towards",
                        "a realistic prediction."
                      )
                    ),
                    div(
                      h4("Model Application"),
                      p(
                        "In presence-only prediction, MaxEnt is used to predict the potential",
                        "distribution of occurrences in unsampled areas based on the environmental",
                        "characteristics of known presence locations. The model outputs a probability",
                        "distribution over the study area, indicating the suitability of each location."
                      )
                    )
                  )
                )
              ),
              conditionalPanel(
                condition="input.menuItemSelected == 'mdlGLMOverviewSelect'",
                div(
                  id="glmOverviewSection",
                  modeling.info.sidebar,
                  div( 
                    class="model-overview-page",
                    h3("Generalized Linear Models (Logistic Regression)"),
                    p(
                      "Generalized Linear Models (GLMs) are a class of models that generalize",
                      "linear regression by allowing for non-normal distributions of the target",
                      "variable. In the context of binary classification, the GLM becomes a",
                      "logistic regression, modeling the log odds of the probability of the event."
                    ),
                    div(
                      h4("Logistic Function"),
                      p(
                        "The logistic function is used to squeeze the output of a linear equation",
                        "between 0 and 1. The equation for the logistic function is:"
                      ),
                      p(
                        tags$span(class="math inline", 
                                  HTML("\\( f(z) = \\frac{1}{1 + e^{-z}} \\)")),
                        HTML("&nbsp;&nbsp;&nbsp;"),  # Non-breaking spaces
                        ", where ", tags$span(class="math inline", HTML("\\(z\\)")), 
                        " is the output of the linear layer of the model."
                      )
                    ),
                    div(
                      h4("Regularization"),
                      p(
                        "Regularization is a technique used to prevent overfitting by adding a",
                        "penalty to the loss function. In the context of the ",
                        tags$code("glmnet"), " package used in this applicaction both L1 (Lasso)",
                        "and L2 (Ridge) regularization are used. The strength and type of",
                        "regularization are controlled by the ", 
                        tags$span(class="math inline", HTML("\\(\\alpha\\)")), " (", tags$code("alpha"),
                        ") and ", tags$span(class="math inline", HTML("\\(\\lambda\\)")), 
                        " (", tags$code("lambda"), ") parameters."
                      )
                    ),
                    div(
                      h4("Model Tuning"),
                      p(
                        "For the tuning grid used in the model in the application, the",
                        "following parameters are iteratively tested (as defined by the user):"
                      ),
                      tags$ul(
                        tags$li(tags$code("alpha"), 
                                "This controls the mix between L1 and L2 regularization. An ",
                                tags$code("alpha"), " of 1 is Lasso regression and 0 is Ridge regression."),
                        tags$li(tags$code("lambda"), 
                                "This is the regularization strength. Larger values of ", 
                                tags$code("lambda"), " increase the regularization effect.")
                      ),
                      p(
                        "The combination of ", tags$code("alpha"), " and ", tags$code("lambda"), 
                        " allows for elastic net regularization, which is a mix of L1 and L2."
                      )
                    )
                  )
                )
                
              ),
              conditionalPanel(
                condition="input.menuItemSelected == 'mdlRFOverviewSelect'",
                id="rfOverviewSection",
                div(
                  modeling.info.sidebar,
                  div( 
                    class="model-overview-page",
                    h3("Classification Trees"),
                    p(
                      "Classification trees are a type of decision tree algorithm that",
                      "aims to classify instances into predefined classes. They work by",
                      "recursively splitting the dataset based on the feature that provides",
                      "the highest information gain, using measures like Gini impurity or",
                      "deviance (cross-entropy)."
                    ),
                    div(
                      h4("Gini Impurity"),
                      p(
                        "The Gini impurity is commonly used in decision trees for binary",
                        "classification. It calculates the impurity (or disorder) of a set,",
                        "given the probability of choosing an item from one class.",
                        "The Gini impurity is 0 when all items belong to a single class,",
                        "and it is maximized when the items are evenly distributed across",
                        "different classes. For a binary classification probelem, the Gini",
                        " impurity measure is defined as:"
                      ),
                      p(
                        tags$span(class="math inline", 
                                  HTML("\\( Gini = 2p(1-p) \\)")),
                        ", where ", tags$span(class="math inline", HTML("\\( p \\)")), " is the",
                        " probability of choosing an item from one class."
                      )
                    ),
                    div(
                      h4("Information Gain"),
                      p(
                        "Information gain is a measure used in decision trees to determine which",
                        "feature to split on. It represents the difference between the impurity of",
                        "the original set and the weighted impurity of the two child sets after",
                        "the split. The greater the information gain, the more effective the split.",
                        "For binary classification, information gain can be calculated using the",
                        "Gini impurity measure:"
                      ),
                      tags$div(
                        tags$span(
                          class="math inline", 
                          HTML("\\( IG_{Gini}(D, A) = Gini(D) - \\left( \\frac{|D_1|}{|D|} ",
                               "\\cdot Gini(D_1) + \\frac{|D_2|}{|D|} \\cdot Gini(D_2) \\right) \\)")),
                        p("Given:"),
                        tags$ul(
                          tags$li(
                            HTML("\\( Gini(D) \\) is the Gini impurity of the dataset \\( D \\) ",
                                 "before the split.")),
                          tags$li(HTML("\\( Gini(D_1) \\) and \\( Gini(D_2) \\) are the Gini ",
                                       "impurities of the two subsets after the split.")),
                          tags$li(HTML("\\( |D_1| \\) and \\( |D_2| \\) are the sizes of the ",
                                       "two subsets after the split.")),
                          tags$li(HTML("\\( |D| \\) is the size of the dataset \\( D \\) ",
                                       "before the split."))
                        )
                      )
                    ),
                    div(
                      h4("Model Tuning"),
                      p(
                        "Only one parameter is defined by the user in this application when ",
                        "tuning a classification tree. The complexity parameter (", tags$code("cp"), 
                        ") is used to control the size of the decision tree and prevent overfitting.", 
                        " It specifies a threshold below which a node split is considered too small. ",
                        "If the difference in the impurity measure (e.g., Gini) of a split is below",
                        "this threshold, the split is not made, resulting in a more generalized tree."
                      )
                    ),
                    h3("Random Forests"),
                    p(
                      "A random forest is an ensemble machine learning model that combines",
                      "the predictions of several decision trees to improve predictive accuracy.",
                      "Unlike boosting, the trees in a random forest are trained independently.",
                      "Each tree is trained on a different bootstrap sample of the data (a sample",
                      "drawn with replacement), and at each node, a random subset of features is",
                      "considered for splitting. This randomness helps to make the model robust",
                      "to overfitting and improves predictive accuracy by reducing the correlation",
                      "between the trees."
                    ),
                    p(
                      "Random forests are known for their robustness and versatility. They can",
                      "handle both numerical and categorical data, they don’t require feature", 
                      "scaling, and they can model complex non-linear relationships. However,",
                      "they can be computationally intensive to train, and their predictions",
                      "are not as interpretable as those of a single decision tree."
                    ),
                    div(
                      h4("Extremely Randomized Trees (ExtraTrees)"),
                      p(
                        "The “extratrees” rule introduces additional randomness into the",
                        "tree-building process. Instead of computing the best split point",
                        "for a feature, a random split point is chosen. This added randomness",
                        "can sometimes result in better generalization to unseen data."
                      ),
                      div(
                        h4("Out-of-Bag Error"),
                        p(
                          "One of the advantages of random forests is the ability to compute",
                          "an out-of-bag (OOB) error estimate. This is the average error for",
                          "each observation calculated using predictions from the trees that",
                          "do not contain that observation in their bootstrap sample. It's an",
                          "unbiased estimate of the test set error."
                        ),
                        tags$span(
                          class="math inline", 
                          HTML("\\( OOB = \\frac{1}{N} \\sum_{i=1}^{N} I(y_i \\neq \\hat{y}_i) \\)"))
                      ),
                      h4("Model Tuning"),
                      p(
                        "For the tuning grid used in the model in the application, the",
                        "following parameters are iteratively tested (as defined by the user):"
                      ),
                      tags$ul(
                        tags$li(tags$code("mtry"), 
                                "This is the number of variables randomly sampled as candidates",
                                "at each split. Lower values can make the model more robust to",
                                "overfitting by adding randomness."),
                        tags$li(tags$code("splitrule"), 
                                "This is the splitting rule or criterion. The “gini” rule is for",
                                "classification tasks and uses the Gini impurity as the splitting", 
                                "criterion. The “extratrees” rule implements the extremely",
                                "randomized trees algorithm, which adds extra randomness to the", 
                                "model."),
                        tags$li(tags$code("min.node.size"), 
                                "This is the minimum size of terminal nodes. Nodes with fewer",
                                "observations than ", tags$code("min.node.size"), 
                                "will not be split further.",
                                "Larger values can help prevent overfitting by",
                                "making the model more conservative.")
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        tabPanel(
          "Model Fitting",
          div(class="spinner-placeholder", 
              style='display:none; margin-top:40%; position:relative;',
              HTML('<div></div> <div></div> <div></div> <div></div>')
          ),
          # Train/Test Split
          div(
            id="modelFittingSection",
            h3("Fit Custom Models"),
            div(
              id="modelFittingSharedParams",
              class="shiny-row",
              div(
                div(style="height:26px;"),
                div(
                  style="height:28px; width:80px; text-align:right;
                                 margin:4px; margin-bottom:14px; margin-top:0;", 
                  span(style="color:red", "* required")),
              ),
              div(
                style="max-width:150px;",
                numericInput("split_perc", "Split %", value=0.75, min=0.5, max=0.99, step=0.1)
              ),
              div(
                div(style="height:26px;"),
                div(
                  style="height:28px; width:80px; text-align:right; margin-bottom:14px; ", 
                  span(style="color:red", "* required")),
              ),
              div(
                style="max-width:150px; margin-left:4px;",
                numericInput("k_folds", "Cross Validation Folds", value=5, min=3, max=10, step=1)
              )
            ),
            div(
              class="shiny-row",
              id="modelFittingParams",
              # GLM/Logistic Regression
              # Optionally apply regularization
              div(
                style="width:365px;",
                id="modelFittingGLM",
                div(
                  style="padding:10px; padding-left:20px; padding-top:0; max-width:300px;",
                  h3("GLM (Logistic Regression)"),
                  tags$i("When Alpha=0 and Lambda=0 (no regularization), this model is 
                            just a traditional Logistic Regression.")
                  # TODO: Describe L1/L2/Elastic Net Regularization here
                ),
                # Params: alpha, lambda
                # Allow for up to 9 combinations (3 of each)
                div(
                  tags$ul(
                    style="list-style-type: none; padding: 0; margin: 0;",
                    tags$li(
                      class="shiny-row",
                      div(style="width:80px; margin:2px;"),
                      div(style="width:110px; margin:2px;", h4("Alpha")),
                      div(style="width:110px; margin:2px;", h4("Lambda"))
                    ),
                    tags$li(
                      div(
                        class="shiny-row",
                        div(style="width:80px; text-align:right;  margin:2px;", 
                            span(style="color:red", "* required")),
                        div(style="width:110px;  margin:2px;",
                            numericInput("glm_alpha_1", label=NULL, value=0.0, 
                                         min=0.0, max=1.0, step=0.1)),
                        div(style="width:110px;  margin:2px;",
                            numericInput("glm_lambda_1", label=NULL, value=0.0, min=0.0, step=0.1))
                      ),
                      div(
                        class="shiny-row",
                        div(style="width:80px; text-align:right;  margin:2px;", 
                            tags$i("optional")),
                        div(style="width:110px;  margin:2px;", 
                            numericInput("glm_alpha_2", label=NULL, value=NULL, 
                                         min=0.0, max=1.0, step=0.1)),
                        div(style="width:110px;  margin:2px;", 
                            numericInput("glm_lambda_2", label=NULL, value=NULL, min=0.0, step=0.1))
                      ),
                      div(
                        class="shiny-row",
                        div(style="width:80px; text-align:right;  margin:2px;", 
                            tags$i("optional")),
                        div(style="width:110px;  margin:2px;",
                            numericInput("glm_alpha_3", label=NULL, value=NULL, 
                                         min=0.0, max=1.0, step=0.1)),
                        div(style="width:110px;  margin:2px;",
                            numericInput("glm_lambda_3", label=NULL, value=NULL, min=0.0, step=0.1))
                      )
                    )
                  )
                )
              ),
              # Decision Tree
              div(
                style="width:290px;",
                id="modelFittingTree",
                div(
                  style="padding:10px; padding-left:0; padding-top:0; max-width:250px;",
                  h3("Classification Tree"),
                  tags$i("Description")
                ),
                
                # Params: cp
                # Up to 10 combinations of the complexity parameter
                div(
                  h4("Complexity Parameter Value Range"),
                  div(
                    style="max-width:230px;",
                    div(
                      class="shiny-row",
                      div(
                        div(style="height:26px;"),
                        div(
                          style="height:28px; width:80px; text-align:right;
                                 margin:4px; margin-bottom:14px; margin-top:0;", 
                          span(style="color:red", "* required")),
                      ),
                      numericInput("start_cp", "Minumum Value", value=0, min=0, max=100, step=0.1)
                      ),
                    div(
                      class="shiny-row",
                      div(
                        div(style="height:26px;"),
                        div(
                          style="height:28px; width:80px; text-align:right;
                                 margin:4px; margin-bottom:14px; margin-top:0;", 
                          span(style="color:red", "* required")),
                      ),
                      numericInput("range_cp", "Values in Range", value=1, min=1, max=10, step=1)
                    ),
                    div(
                      class="shiny-row",
                      div(
                        div(style="height:26px;"),
                        div(
                          style="height:28px; width:80px; text-align:right;
                                 margin:4px; margin-bottom:14px; margin-top:0;", 
                          span(style="color:red", "* required")),
                      ),
                      numericInput("step_cp", "Step Size", value=0.1, min=0.1, max=100, step=0.1)
                    )
                  )
                )
              ),
              # Random Forest
              # Params: mtry, splitrule, min.node.size
              # Up to 18 combinations
              div(
                style="width:375px;",
                id="modelFittingForest",
                div(
                  style="padding:10px; padding-left:0; padding-top:0; max-width:365px;",
                  h3("Random Forest"),
                  tags$i("Description")
                ),
                div(
                  style="max-width:210px; ",
                  div(
                    id="splitDiv",
                    class="shiny-row",
                    div(
                      div(style="height:26px;"),
                      div(
                        style="height:28px; width:60px; text-align:right;
                                 margin-right:4px; margin-bottom:4px; margin-top:0;", 
                        span(style="color:red", "* required")),
                    ),
                    selectInput("split_rule", "Split Rule", 
                                choices=c("gini", "extratrees"), 
                                selected="gini", multiple=T,
                                width="150px")
                  )
                ),
                div(
                  tags$ul(
                    style="list-style-type: none; padding: 0; margin: 0;",
                    tags$li(
                      id="rfParams",
                      class="shiny-row",
                      div(style="width:60px; margin:2px;"),
                      div(style="width:150px; margin:2px;", h4("# of Splits per Node")),
                      div(style="width:150px; margin:2px;", h4("Minimal Node Size"))
                    ),
                    tags$li(
                      div(
                        class="shiny-row",
                        div(style="width:60px; text-align:right;  margin:2px;", 
                            span(style="color:red", "* required")),
                        div(style="width:150px; margin:2px;",
                            numericInput("mtry_1", label=NULL, value=1, min=1, max=11, step=1)),
                        div(style="width:150px;  margin:2px;",
                            numericInput("min_node_1", label=NULL, value=1, min=1, max=10, step=1))
                      ),
                      div(
                        class="shiny-row",
                        div(style="width:60px; text-align:right;  margin:2px;", 
                            tags$i("optional")),
                        div(style="width:150px;  margin:2px;",
                            numericInput("mtry_2", label=NULL, value=NULL, min=1, max=11, step=1)),
                        div(style="width:150px;  margin:2px;",
                            numericInput("min_node_2", label=NULL, value=NULL, min=1, max=10, step=1))
                      ),
                      div(
                        class="shiny-row",
                        div(style="width:60px; text-align:right;  margin:2px;", 
                            tags$i("optional")),
                        div(style="width:150px;  margin:2px;",
                            numericInput("mtry_3", label=NULL, value=NULL, min=1, max=11, step=1)),
                        div(style="width:150px;  margin:2px;",
                            numericInput("min_node_3", label=NULL, value=NULL, min=1, max=10, step=1))
                      )
                    )
                  )
                )
              )
            ),
            div(
              class="shiny-row",
              div(
                style="margin-left:15px;",
                actionButton("apply_model_updates", "Apply Updates & Fit Models")
              ),
              div(
                style="margin-left:15px;",
                actionButton("undo_model_updates", "Undo Changes")
              ),
              div(
                style="margin-left:15px;",
                actionButton("reset_model_updates", "Reset to Default")
              )
            ),
            br()
          )
        ),
        tabPanel(
          "Prediction & Model Evaluation",
          div(
            class="shiny-row",
            div(
              id="modelEvalSelection",
              class="shiny-row",
              div(
                id="radioSection",
                style="max-width:175px;",
                radioButtons("viz_preds", label="Select View", 
                             choices=c("Model Metrics", "Estimated Probability Raster", 
                                       "Predicted vs. Actual Map", "Probability Density Plot", 
                                       "Bar Plot", "Confusion Matrix"), 
                             selected="Model Metrics", 
                             inline=F),
                conditionalPanel(
                  condition="input.viz_preds != 'Estimated Probability Raster' && 
                         input.viz_preds != 'Probability Density Plot'",
                  div(
                    style="max-width:170px;",
                    # Select Cutoff (default to 0.5)
                    selectInput("predictionCutoff", "Select Cutoff", 
                                choices=seq(0.1, 0.9, by=0.1),
                                selected=0.5)
                  )
                )
              ),
              actionButton("toggleRadioBtn", "-", width=30, height=10)
            ),
            div(
              id="modelOutputs",
              style="min-height:500px; margin:15px; overflow-x:scroll; 
                     border: 0 solid #888; display: flex; justify-content: center; 
                     align-items: center; position:relative;",
                div(
                  id="spinnerContainer",
                  shinybusy::add_busy_spinner(spin = "circle")
                ),
                conditionalPanel(
                  condition="input.viz_preds == 'Model Metrics'",
                  style="max-width:600px; min-height:500px; min-width:150px;",
                  DTOutput("metric_table")
                ),
                conditionalPanel(
                  condition="input.viz_preds == 'Estimated Probability Raster'",
                  style="min-height:500px; min-width:150px;",
                  plotOutput("raster_estimate")
                ),
                conditionalPanel(
                  condition="input.viz_preds == 'Predicted vs. Actual Map'",
                  style="min-height:500px; min-width:150px;",
                  plotOutput("pred_map")
                ),
                conditionalPanel(
                  condition="input.viz_preds == 'Probability Density Plot'",
                  style="min-height:500px; min-width:150px;",
                  plotOutput("prob_density")
                ),
                conditionalPanel(
                  condition="input.viz_preds == 'Bar Plot'",
                  style="min-height:500px; min-width:150px;",
                  plotOutput("pred_bar")
                ),
                conditionalPanel(
                  condition="input.viz_preds == 'Confusion Matrix'",
                  style="min-height:500px; min-width:150px;",
                  uiOutput("confusion_matrix")
                )
            )
          )
        )
      )
    )
  )
)


main.body <- dashboardBody(
  includeScript("www/scripts.js"),
  includeCSS("www/styles.css"),
  div(
    id="mainBodySection",
    overview.page,
    data.page,
    eda.page,
    model.page
  )
)

ui <- div(
  id="uiBackground",
  class=".load_wrapper",
  tags$head(
    useShinyjs(),  
    extendShinyjs(text=readr::read_file('www/scripts.js'),
                  functions = c("loadingPanel", "finishedLoadingPanel")),
    tags$title("Slothful Seer"),
    tags$link(rel="shortcut icon", href="favicon.ico"),
    tags$head(tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-MML-AM_CHTML")),
  ),
  div(
    id="headerSection",
    class="shiny-row",
    #a(
    ##  href="#",
    #  role="button",
    #  toggle="offcanvas",
    #  class="sidebar-toggle",
    #  id="sidebarToggle",
    #  tags$span(class="sr-only", "Toggle navigation"),
    #  
    #),
    h1(
      id="mainTitle", 
      "Modeling the Distribution of the Three-Toed Sloth")
  ),
  dashboardPage(
    title = "Slothful Seer",
    skin = "black",
    header=dashboardHeader(disable=T),
    sidebar = main.sidebar,
    body = main.body
  )
)
