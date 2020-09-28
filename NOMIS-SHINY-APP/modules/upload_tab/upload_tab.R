


source('./modules/upload_tab/upload_file_tab.R')
source('./modules/upload_tab/upload_data_tab.R')


uploadTabUI <- function(id){
  ns <- NS(id)
  
  tabsetPanel(
    tabPanel(
      'Upload data',
      uploadDataTabUI(ns("upload-data"))
    ),
    tabPanel(
      'Upload data from file',
      uploadFileTabUI(ns("upload-file"))
    )
  )
  
}


uploadTab <- function(input,output,session,pool,dimension){
  
  callModule(uploadFileTab,"upload-file",pool)
  callModule(uploadDataTab,"upload-data",pool,dimension)
}