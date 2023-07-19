# load libraries
suppressWarnings(suppressPackageStartupMessages(library(shiny)))
suppressWarnings(suppressPackageStartupMessages(library(shinydashboard)))
suppressWarnings(suppressPackageStartupMessages(library(shinyjs)))
suppressWarnings(suppressPackageStartupMessages(library(DT)))

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
            id="overviewIntroSection",
            HTML("<p>The purpose of this application is to model the species distribution of 
                  the three-toed sloth using advanced analytical methods and spatial statistics. 
                  This type of model is commonly used in the fields of biology and ecology, and is
                  called <i>presence-only</i> modeling, (since the data only entails observations 
                  of a species presence, and not the inverse). The contents of the app include:</p>"),
            tags$ul(
              tags$li("Basic exploratory analysis: In this part of the application, the user 
                       is able to gain a deeper understanding of the data itself, as well as the
                       distribution of the variables used within the modeling process."),
              tags$li("Splitting the data: As with most supervised learning problems, the data is
                       split into training/test sets in order to evaluate model performance on
                       unseen data."),
              tags$li("Baseline models: Inhomogeneous Poisson Process (IPP) model and Maximum 
                       Entropy (MaxEnt) models serve as \"baseline\" models, given their frequent
                       usage in presence-only prediction problems."),
              tags$li("Machine learning models: Other models used less frequently in 
                      presence-only prediction problems are used to model the data, including
                      Generalized Linear Models (GLM), GLM with elastic net regularization, 
                      K-Nearest Neighbors (KNN), Random Forest, and Bagged AdaBoost"),
              tags$li("Model evaluation: Each of the models is compared and evaluated.")
            )
          )
        ),
        tabPanel(
          "Data Overview",
          div(
            id="dataOverviewSection",
            HTML("<p>The <code>dismo</code> package provides certain built-in datasets that can be 
                  used for demonstration purposes. <i>Note that the built-in datasets might have been
                  scaled or modified. E.g., the temperature raster data used in this analysis 
                  appears to have been scaled since the min/max values range from about 
                 -200 to 400.</i></p>"),
            HTML("<p>The data from the <code>dismo</code> package used in this application 
                 includes:</p>"),
            div(
              class="shiny-row",
              tags$ul(
                tags$li(
                  HTML("<p>Bradypus (Three-Toed Sloth) observations in South America: This 
                      data was obtained from the Global Biodiversity Information Facility 
                      (<a href='https://www.gbif.org/'>GBIF</a>) via the following function 
                      in R: <br><code>dismo::bradypus::gbif(\"Bradypus\", \"variegatus*\", sp=T)
                      </code></p>")
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
                      HTML("<p>Terrestrial Ecosregions (Biomes) of the world,
                         by the World Wildlife Fund (<i>biome</i>).See 
                         <a href='https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world'>this publication</a> by the WWF for more details.</p>")
                    )
                  )
                )
              ),
              div(
                tags$img(src="images/sloth.jpg", alt="Sloth", style="width:300px; height:300px;"),
                span(id="slothCaption", 
                     tags$i(
                       "Image obtained from ", 
                       tags$a(href="https://res.cloudinary.com/dk-find-out/image/upload/q_80,w_1920,f_auto/A-dreamstime_xxl_41801886_by73ed.jpg", "Cloudinary")
                     )
                )
              )
            )
          )  
        ),
        tabPanel("Presence-Only Prediction")
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
        actionButton("export_data", 
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
            radioButtons("map_type", label="Select Map", choices=c("Interactive Map of Points", "Rasters"), 
                         inline=T, selected="Interactive Map of Points"),
            conditionalPanel(
              "input.map_type == 'Rasters'",
              div(
                class="shiny-row",
                selectInput("map_selection", label="Select Raster", selected="Mean Temperature", 
                            choices=c("Mean Temperature", "Annual Precipitation", 
                                      "Precipitation - Wettest Qrtr.", "Precipitation - Driest Qrtr.",
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
                style="overflow-x:scroll; min-width:600px; margin-top:15px; margin-left:15px;",
                "input.cont_plot_type == 'Table'",
                div(style="margin-bottom:6px;", DTOutput("cont_table"))
              ),
              conditionalPanel(
                style="overflow-x:scroll; min-width:780px; margin-top:15px; margin-left:15px;",
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
          "Modeling Info",
          div(
            class="shiny-row",
            div(
              id="modelingInfoSidebar",
              tags$ul(
                tags$li(
                  tags$a(
                    id="mdlOverviewSelect",
                    tags$i(icon("circle-info"), style="margin-right:10px"),
                    span("Modeling Overview")
                  )
                ),
                # Train/Test Split
                tags$li(
                  tags$a(
                    id="mdlTTOverviewSelect",
                    tags$i(icon("shuffle"), style="margin-right:10px"),
                    span("Train/Test Splitting")
                  )
                ),
                # IPP
                tags$li(
                  tags$a(
                    id="mdlIPPOverviewSelect",
                    tags$i(icon("line-chart"), style="margin-right:10px"),
                    span("Inhomogeounous Poisson Process")
                  )
                ),
                # MaxEnt
                tags$li(
                  tags$a(
                    id="mdlMEOverviewSelect",
                    tags$i(icon("maximize"), style="margin-right:10px"),
                    span("Maximum Entropy")
                  )
                ),
                # GLM
                tags$li(
                  tags$a(
                    id="mdlGLMOverviewSelect",
                    tags$i(icon("bar-chart"), style="margin-right:10px"),
                    span("GLM")
                  )
                ),
                # Classification Tree
                tags$li(
                  tags$a(
                    id="mdlCTOverviewSelect",
                    tags$i(icon("arrows-split-up-and-left"), style="margin-right:10px"),
                    span("Classification Tree")
                  )
                ),
                # Random Forest
                tags$li(
                  tags$a(
                    id="mdlRFOverviewSelect",
                    tags$i(icon("tree"), style="margin-right:10px"),
                    span("Random Forest")
                  )
                )
              )
            ),
            div(
              conditionalPanel(
                condition="input.menuItemSelected == 'mdlOverviewSelect'",
                div(
                  h3("Model Info: Overview")
                )
              ),
              conditionalPanel(
                condition="input.menuItemSelected == 'mdlTTOverviewSelect'",
                div(
                  h3("Splitting the Data into Train/Test Sets")
                )
              ),
              conditionalPanel(
                condition="input.menuItemSelected == 'mdlIPPOverviewSelect'",
                div(
                  id="ippOverviewSection",
                  h3("Inhomogeneous Poisson Process (IPP)"),
                  p("An Inhomogeneous Poisson Process (IPP) model is a statistical model used 
                  for events that occur randomly over space or time. Unlike a regular Poisson
                  process, the rate of event occurrence in an IPP can vary. IPP models are 
                  often used in presence-only problems to model the intensity of events
                  across different locations or times. In the context of this application, the 
                  IPP model is defined as:"),
                  tags$ul(
                    tags$li(
                      p(
                        "IPP Function: In the case of the IPP model in this application, the
                        intensity", tags$span(class="math inline", HTML("\\(\\lambda\\)")),
                        "is not a constant, but is a function of the location and covariates ",
                        tags$span(class="math inline", HTML("\\(x\\)")),
                        ". Hence the term ‘inhomogeneous’."
                      ),
                      p(tags$span(class="math inline", "\\(\\lambda(x) = N \\cdot g(x)\\)"), 
                        ", where ", tags$span(class="math inline", HTML("\\(N\\)")), 
                        " is the total number of events, and ", tags$span(class="math inline", 
                                                                          HTML("\\(g(x)\\)")),
                        " is the density function.")
                    ),
                    tags$li(
                      "Probability Density Function (PDF): ",
                      p(tags$span(class="math inline", 
                                  "\\[f(x) = \\lambda e^{-\\lambda x}, \\quad x \\geq 0\\]"))
                    ),
                    tags$li(
                      "Cumulative Distribution Function (CDF):",
                      p(tags$span(class="math inline", 
                                  HTML("\\[F(x) = 1 - e^{-\\lambda x}, \\quad x \\geq 0\\]"))),
                      
                    ),
                    tags$li(
                      p("By fitting an IPP to presence-only data, we estimate the underlying 
                  intensity function ", tags$span(class="math inline", 
                                                  HTML("\\(\\lambda(x)\\)")),
                        ". This informs us about how the rate of event occurrence — in this 
                  case, the number of three-toed sloth counts — changes across different 
                  times or locations.")
                    ),
                    tags$li(
                      p("The fitted model predicts the count of sloths at each point. Because the 
                   other models in this application model probabilities, an extra step is 
                   needed to calculate the probability at each point that the count is at least 1:"),
                      p(
                        tags$span(class="math inline", HTML("\\(P(At\\ least\\ 1) = 1 - e^{-x}\\)")),
                        ", where ", 
                        tags$span(class="math inline", HTML("\\(x\\)")), 
                        " represents the predicted values from the intensity function ",
                        tags$span(class="math inline", HTML("\\(\\lambda(x)\\)")),
                        "for each location in the raster.")
                    ),
                    tags$li(
                      "IPP Assumptions:",
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
                )
              ),
              conditionalPanel(
                condition="input.menuItemSelected == 'mdlMEOverviewSelect'",
                div(
                  h3("Maximum Entropy")
                )
              ),
              conditionalPanel(
                condition="input.menuItemSelected == 'mdlGLMOverviewSelect'",
                div(
                  h3("GLM")
                )
              ),
              conditionalPanel(
                condition="input.menuItemSelected == 'mdlCTOverviewSelect'",
                div(
                  h3("Classification Trees")
                )
              ),
              conditionalPanel(
                condition="input.menuItemSelected == 'mdlRFOverviewSelect'",
                div(
                  h3("Random Forests")
                )
              )
            )
          )
        ),
        tabPanel(
          "Model Fitting"
          # Train/Test Split
          # # IPP
          # MaxEnt
          # Logistic Regression
          # Optionally apply regularization
          # Decision Tree
          # Random Forest
        ),
        tabPanel(
          "Prediction"
        ),
        tabPanel(
          "Model Evaluation and Comparison"
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
  tags$head(
    useShinyjs(),  
    tags$title("Slothful Seer"), #🦥
    tags$link(rel="shortcut icon", href="favicon.ico"),
    tags$head(tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-MML-AM_CHTML")),
    
  ),
  div(
    id="headerSection",
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
