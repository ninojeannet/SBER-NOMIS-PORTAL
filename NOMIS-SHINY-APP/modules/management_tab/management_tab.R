
source('./modules/management_tab/management_data_tab.R')
source('./modules/management_tab/management_process_tab.R')


managementTabUI <- function(id) {
  # Parameters:
  #  - id: String, the module id
  ns <- NS(id)
  
  tabsetPanel(
    tabPanel(
      'Manage data',
      managementDataTabUI(ns("data"))
  
    ),
    tabPanel(
      'Process data',
      managementProcessTabUI(ns("process"))
    )
  )
}

managementTab <- function(input, output, session,pool){
  callModule(managementDataTab,"data",pool)
  callModule(managementProcessTab,"process",pool)
  
}


