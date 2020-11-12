source('./modules/download_tab/download_data_tab.R')
source('./modules/download_tab/download_file_tab.R')


# Download data tab
# This tab is split in two panel : data download and file download

# UI part of download module
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

# Server side of download module
downloadTab <- function(input,output,session,pool){
  
  callModule(downloadDataTab,"data",pool)
  # callModule(downloadFileTab,"file",pool)
  
}