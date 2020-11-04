source('./modules/download_tab/download_data_tab.R')
source('./modules/download_tab/download_file_tab.R')


downloadTabUI <- function(id){
  ns <- NS(id)
  
  tabsetPanel(
    tabPanel(
      'Download data',
      downloadDataTabUI(ns("data"))
    )
    # ,
    # tabPanel(
    #   'Download file',
    #   downloadFileTabUI(ns("file"))
    # )
  )
  
}


downloadTab <- function(input,output,session,pool){
  
  callModule(downloadDataTab,"data",pool)
  # callModule(downloadFileTab,"file",pool)
  
}