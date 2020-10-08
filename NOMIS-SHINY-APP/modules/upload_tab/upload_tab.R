


source('./modules/upload_tab/upload_expedition_tab.R')
source('./modules/upload_tab/upload_data_tab.R')


uploadTabUI <- function(id){
  ns <- NS(id)
  
  # panel(
  #   'Upload data',
    # uploadDataTabUI(ns("data"))
  # )
  tabsetPanel(
    tabPanel(
      'Upload data',
      uploadDataTabUI(ns("data"))

  ),
  tabPanel(
    'Create a new expedition',
    uploadExpeditionTabUI(ns("expedition"))
  )
  )
  
}


uploadTab <- function(input,output,session,pool,dimension){
  
  callModule(uploadExpeditionTab,"expedition",pool)
  callModule(uploadDataTab,"data",pool,dimension)
}