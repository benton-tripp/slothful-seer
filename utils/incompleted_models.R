incompleted.models.modal <- function() {
  showModal(
    modalDialog(
      title="Error Finding Trained Models:",
      size="m",
      easyClose=T,
      div(
        style="font-size:18px; margin:10px;",
        span(
          tags$i("There are either no fitted models, or there is at least one model
                      that was trained unsuccessfully due to a bad parameter input. Please
                      ensure that all models have run successfully from the \"Model Fitting\" 
                      tab prior to computing predictions and evaluating/comparing the 
                      models.")
        )
      )
    )
  )
}