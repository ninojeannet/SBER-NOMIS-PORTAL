source('./modules/data_module/manage_data_tab.R')


uploadTabUI <- function(id){
  ns <- NS(id)
  
  # panel(
  #   'Upload data',
    # uploadDataTabUI(ns("data"))
  # )
  tabsetPanel(
    tabPanel(
      'Upload data',
      manageDataTabUI(ns("data"))

  )
  )
  
}


uploadTab <- function(input,output,session,pool,dimension){
  
  callModule(manageDataTab,"data",pool,dimension,TRUE)
}