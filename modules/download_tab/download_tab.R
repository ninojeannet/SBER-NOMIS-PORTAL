source('./modules/download_tab/download_data_tab.R')
source('./modules/download_tab/download_file_tab.R')


# Download data tab
# This tab is split in two panel : data download and file download

# UI part of download module
downloadTabUI <- function(id,pool){
  ns <- NS(id)
  
  tabsetPanel(
    tabPanel(
      'Download data',
      downloadDataTabUI(ns("data"),pool)
    )
    ,
    tabPanel(
      'Download file',
      downloadFileTabUI(ns("file"),pool)
    )
  )
  
}

# Server side of download module
downloadTab <- function(input,output,session,pool){
  
  callModule(downloadDataTab,"data",pool)
  callModule(downloadFileTab,"file",pool)
  
}