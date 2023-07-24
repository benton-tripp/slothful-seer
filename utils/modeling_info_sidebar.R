modeling.info.sidebar <- div(
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
        tags$i(icon("arrows-split-up-and-left"), style="margin-right:10px"),
        span("Train/Test Splitting, and Model Training")
      )
    ),
    # IPP
    tags$li(
      tags$a(
        id="mdlIPPOverviewSelect",
        tags$i(icon("line-chart"), style="margin-right:10px"),
        span("Inhomogeounous Poisson Process (IPP)")
      )
    ),
    # MaxEnt
    tags$li(
      tags$a(
        id="mdlMEOverviewSelect",
        tags$i(icon("maximize"), style="margin-right:10px"),
        span("Maximum Entropy (MaxEnt)")
      )
    ),
    # GLM
    tags$li(
      tags$a(
        id="mdlGLMOverviewSelect",
        tags$i(icon("bar-chart"), style="margin-right:10px"),
        span("Generalized Linear Models (GLM)")
      )
    ),
    # Classification Tree & Random Forest
    tags$li(
      tags$a(
        id="mdlRFOverviewSelect",
        tags$i(icon("tree"), style="margin-right:10px"),
        span("Classification Trees & Random Forests")
      )
    )
  )
)