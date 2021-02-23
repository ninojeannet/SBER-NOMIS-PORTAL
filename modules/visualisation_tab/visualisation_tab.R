source('./modules/visualisation_tab/visualisation_versus_tab.R')
source('./modules/visualisation_tab/visualisation_simple_tab.R')

visualisationTabUI <- function(id,pool) {
  # Parameters:
  #  - id: String, the module id
  ns <- NS(id)
  
  tabsetPanel(
    # And the map tab
    tabPanel(
      # Tab title
      'Map',
      # Tab content
      htmlTemplate('./html_components/map.html'),
      value = ns('mapTab')
    ),
    tabPanel(
      'Histogram',
      visualisationSimpleTabUI(ns("simple"),pool)
    ),
    tabPanel(
      'Scatter plot',
      visualisationVersusTabUI(ns("versus"),pool)
    )
  )
}

visualisationTab <- function(input, output, session,pool){
  callModule(visualisationVersusTab,"versus",pool)
  callModule(visualisationSimpleTab,"simple",pool)

}
