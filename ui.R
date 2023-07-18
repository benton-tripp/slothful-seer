# load libraries
suppressWarnings(suppressPackageStartupMessages(library(shiny)))
suppressWarnings(suppressPackageStartupMessages(library(shinydashboard)))
suppressWarnings(suppressPackageStartupMessages(library(DT)))

main.sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "sidebarMenu",
    menuItem(span("Overview", class="bold-panel-header"), tabName = "overviewPanel", 
             icon = icon("circle-info")),
    menuItem(span("Exploratory Analysis", class="bold-panel-header"), 
             tabName = "edaPanel", icon = icon("map")),
    menuItem(span("Train/Test Splits", class="bold-panel-header"), 
             tabName = "trainTestSplitPanel", icon = icon("arrows-split-up-and-left")),
    menuItem(span("IPP Model", class="bold-panel-header"), 
             tabName = "baselineModelPanel", icon = icon("line-chart")),
    menuItem(span("MaxEnt Model", class="bold-panel-header"), 
             tabName = "maxEntModelPanel", icon = icon("maximize")),
    menuItem(span("ML Models", class="bold-panel-header"), 
             tabName = "mlModelPanel", icon = icon("robot")),
    menuItem(span("Model Comparison", class="bold-panel-header"), 
             tabName = "modelComparisonPanel", icon = icon("chart-bar"))
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
            HTML("<p>The purpose of this application is to model the species distribution of the three-toed sloth using 
               advanced analytical methods and spatial statistics. This type of model is commonly used in the fields of
               biology and ecology, and is called <i>presence-only</i> modeling, (since the data only entails observations 
               of a species presence, and not the inverse). The contents of the app include:</p>"),
            tags$ul(
              tags$li("Basic exploratory analysis: In this section of the application, the user is able to gain a 
                   deeper understanding of the distribution of the variables used within the modeling process."),
              tags$li("Splitting the data: As with most supervised learning problems, data should be split into 
                   training/test sets in order to evaluate model performance on unseen data."),
              tags$li("Baseline models: Inhomogeneous Poisson Process (IPP) model and Maximum Entropy (MaxEnt) models
                    serve as \"baseline\" models, given their frequent usage in presence-only prediction problems."),
              tags$li("Machine learning models: Other models used less frequently in presence-only prediction problems
                   are used to model the data, including Generalized Linear Models (GLM), GLM with elastic net 
                   regularization, K-Nearest Neighbors (KNN), Random Forest, and Bagged AdaBoost"),
              tags$li("Model evaluation: Each of the models can be compared and evaluated.")
            )
          )
        ),
        tabPanel("Data Overview"),
        tabPanel("Presence-Only Prediction"),
        tabPanel(
          "Inhomogeonous Poisson Process",
          div(
            id="ippOverviewSection",
            p("An Inhomogeneous Poisson Process (IPP) is a statistical model used for events that occur randomly over 
            space or time. Unlike a regular Poisson process, the rate of event occurrence in an IPP can vary. IPP models are
            often used in presence-only problems to model the intensity of events across different locations or times.
            In the context of this application, the IPP model is defined as:"),
            tags$ul(
              tags$li(
                p(
                  "IPP Function: In the case of the IPP model in this application, the intensity", 
                  tags$span(class="math inline", HTML("\\(\\lambda\\)")),
                  "is not a constant, but is a function of the location and covariates ",
                  tags$span(class="math inline", HTML("\\(x\\)")),
                  ". Hence the term ‘inhomogeneous’."
                ),
                p(tags$span(class="math inline", "\\(\\lambda(x) = N \\cdot g(x)\\)"), 
                  ", where ", tags$span(class="math inline", HTML("\\(N\\)")), 
                  " is the total number of events, and ", tags$span(class="math inline", HTML("\\(g(x)\\)")),
                  " is the density function.")
              ),
              tags$li(
                "Probability Density Function (PDF): ",
                p(tags$span(class="math inline", "\\[f(x) = \\lambda e^{-\\lambda x}, \\quad x \\geq 0\\]"))
              ),
              tags$li(
                "Cumulative Distribution Function (CDF):",
                p(tags$span(class="math inline", HTML("\\[F(x) = 1 - e^{-\\lambda x}, \\quad x \\geq 0\\]"))),
                
              ),
              tags$li(
                p("By fitting an IPP to presence-only data, we estimate the underlying intensity function ",
                  tags$span(class="math inline", HTML("\\(\\lambda(x)\\)")),
                  ". This informs us about how the rate of event occurrence— in this case, the number of three-toed 
                  sloth counts— changes across different times or locations.")
              ),
              tags$li(
                p("The fitted model predicts the count of sloths at each point. Because the other models 
                   in this application model probabilities, an extra step is needed to calculate 
                   the probability at each point that the count is at least 1:"),
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
                    "Independence: It is assumed that the events occur independently in space or time, meaning 
                    the occurrence of an event at one location or time does not affect the occurrence of an 
                    event at another location or time."
                  ),
                  tags$li(
                    "Inhomogeneity: It is assumed that the intensity function can vary across space or time, as 
                    opposed to a homogeneous poisson process, which assumes a constant rate of event occurrence."
                  ),
                  tags$li(
                   "Known Intensity Function: An IPP assumes that the form of the intensity function is known, although 
                   the parameters of the function need to be estimated from the data. This assumption can be violated 
                   if the true intensity function is not well-captured by the chosen form." 
                  ),
                  tags$li(
                    "Complete Spatial Coverage: An IPP assumes that the entire study area has been surveyed and that 
                    presence data is available for all locations where the species is present. This assumption can 
                    be violated due to incomplete or biased sampling."
                  )
                )
              )
            )
          )
        ),
        tabPanel(
          "Maximum Entropy",
          div(
            
          )
        ),
        tabPanel(
          "Other ML Algorithms",
          div(
            
          )
        )
      )
    )
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
          "Data",
          div(
            id="edaDataSection"
          )
        ),
        tabPanel(
          "Data Summary",
          div(
            id="edaSummarySection"
          )
        ),
        tabPanel(
          "Map",
          div(
            id="edaMapSection"
          )
        ),
        tabPanel(
          "Continuous Data",
          div(
            id="edaContSection"
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
                             choices=c("bar", "pie"), selected="bar"),
                plotOutput("cat_plot")
              )
            )
          )
        ),
        tabPanel(
          "Target Variable",
          div(
            id="edaTargetSection"
          )
        )
      )
    )
  )
)

train.test.page <- conditionalPanel(
  condition="input.sidbarMenu == 'trainTestSplitPanel'"
)

baseline.model.page <- conditionalPanel(
  condition="input.sidbarMenu == 'baselineModelPanel'"
)

maxent.page <- conditionalPanel(
  condition="input.sidbarMenu == 'maxEntModelPanel'"
)

ml.models.page <- conditionalPanel(
  condition="input.sidbarMenu == 'mlModelPanel'"
)

model.comp.page <- conditionalPanel(
  condition="input.sidbarMenu == 'modelComparisonPanel'"
)

main.body <- dashboardBody(
  
  includeCSS("www/styles.css"),
  div(
    id="mainBodySection",
    overview.page,
    eda.page,
    train.test.page,
    baseline.model.page,
    maxent.page,
    ml.models.page,
    model.comp.page
  )
)

ui <- div(
  id="uiBackground",
  tags$head(
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
