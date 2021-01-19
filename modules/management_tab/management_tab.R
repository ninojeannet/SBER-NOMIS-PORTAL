
source('./modules/management_tab/management_process_tab.R')
source('./modules/management_tab/management_expedition_tab.R')
source('./modules/data_module/manage_data_tab.R')


managementTabUI <- function(id) {
  # Parameters:
  #  - id: String, the module id
  ns <- NS(id)
  
  tabsetPanel(
    tabPanel(
      'Manage data',
      manageDataTabUI(ns("data"),"Manage data")
      
    ),
    tabPanel(
      'Manage expeditions',
      managementExpeditionTabUI(ns("expedition"))
    ),
    tabPanel(
      'Process data',
      managementProcessTabUI(ns("process"))
    )
  )
}

managementTab <- function(input, output, session,pool,dimension){
  callModule(manageDataTab,"data",pool,dimension,FALSE)
  callModule(managementProcessTab,"process",pool)
  callModule(managementExpeditionTab,"expedition",pool)
  
  
}


