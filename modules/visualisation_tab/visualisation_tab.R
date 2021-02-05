source('./modules/visualisation_tab/visualisation_versus_tab.R')
source('./modules/visualisation_tab/visualisation_simple_tab.R')


visualisationTabUI <- function(id,pool) {
  # Parameters:
  #  - id: String, the module id
  ns <- NS(id)
  
  tabsetPanel(
    tabPanel(
      'Visualisation versus',
      visualisationVersusTabUI(ns("versus"),pool)
    ),
  tabPanel(
      'Visualisation simple',
      visualisationSimpleTabUI(ns("simple"),pool)
    )
  )
  # div(
  #   "functionality not available yet..."
  # )
}

visualisationTab <- function(input, output, session,pool){
  callModule(visualisationVersusTab,"versus",pool)
  callModule(visualisationSimpleTab,"simple",pool)
}
