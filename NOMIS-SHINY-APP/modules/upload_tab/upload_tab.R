source('./modules/data_module/manage_data_tab.R')

# UI side og the uploadTab module
# Parameters : 
# id : id of the module
uploadTabUI <- function(id){
  ns <- NS(id)
  
  tabsetPanel(
    tabPanel(
      'Upload data',
      manageDataTabUI(ns("data"))
      
    )
  )
  
}

# Server function of the uploadTab module
# Parameters : 
# input : input of the shiny app
# output : output of the shiny app
# session : session of the shiny app
# pool : connection pool to access the database
# dimension : the dimension of the window
uploadTab <- function(input,output,session,pool,dimension){
  
  callModule(manageDataTab,"data",pool,dimension,TRUE)
}