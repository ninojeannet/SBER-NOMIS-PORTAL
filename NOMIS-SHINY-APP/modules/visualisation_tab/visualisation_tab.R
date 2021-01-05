

visualisationTabUI <- function(id) {
  # Parameters:
  #  - id: String, the module id
  ns <- NS(id)
  
  tabsetPanel(
    tabPanel(
      'Visualisation',
      manageDataTabUI(ns("data"),"Manage data")
    )
    # ),
    # tabPanel(
    #   'Manage expeditions',
    #   managementExpeditionTabUI(ns("expedition"))
    # ),
    # tabPanel(
    #   'Process data',
    #   managementProcessTabUI(ns("process"))
    # ),
    # tabPanel(
    #   'Project progression',
    #   managementProgressTabUI(ns("progress"))
    # )
  )
}

visualisationTab <- function(input, output, session,pool){
  # callModule(manageDataTab,"data",pool,dimension,FALSE)
  # callModule(managementProcessTab,"process",pool)
  # callModule(managementProgressTab,"progress",pool)
  # callModule(managementExpeditionTab,"expedition",pool)
  
  
}
