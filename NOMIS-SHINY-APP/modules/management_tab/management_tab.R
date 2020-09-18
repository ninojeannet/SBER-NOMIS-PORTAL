## This module contains the UI and server code for the Management tab


source('./utils/template_config.R')

## Create module UI ###############################################################

managementTabUI <- function(id) {
# Create the UI for the downloadTab module
# Parameters:
#  - id: String, the module id
  ns <- NS(id)
  
  # Create namespace
  
  # # Create the sidebarLayout
  # sidebarLayout(
  #   # Create a sidebar with the innerModule first unit input UI elements inside
  #   sidebarPanel(
  #     id = ns('sidebar-management'),
  #     managementSidebarUI(ns("sidebar")),
  #     width = 4
  #   ),
  #   # Create the main panel with the innerModule first unit plot UI elements inside
  #   mainPanel(
  #     id = ns('main-management'),
  #     managementMainUI(ns("main")),
  #     width = 8
  #   )
  # )
  
  # Create the sidebarLayout
  sidebarLayout(
    # Create a sidebar with the innerModule first unit input UI elements inside
    sidebarPanel(
      id = ns('sidebar-management'),
      div(
        selectInput(ns("type"),label = "Select a data type",choices = c("Glacier" = "glacier","Location"="location","Patch"="patch","Enzyme"="enzyme")),
        textInput(ns("searchInput"),"ID"),
        actionButton(ns("search"),"Search"),
      ),
      width = 4
    ),
    # Create the main panel with the innerModule first unit plot UI elements inside
    mainPanel(
      id = ns('main-management'),
      div(
        dataTableOutput(ns("outputTable")),
      ),
      width = 8
    )
  )

}

managementTab <- function(input, output, session,pool){
  # vars <- callModule(managementSidebar,"sidebar")
  # callModule(managementMain,"main",vars,pool)
  
  observeEvent(input$search,{
    output$outputTable <- DT::renderDataTable({
      type <- isolate(input$type)
      column <- templateFieldNames[[type]][1]
      searchInput <- isolate(input$searchInput)
      data <- getData(pool,type,column,searchInput)
      table<-DT::datatable(data,rownames = FALSE,
                           options = list(searching = FALSE, lengthChange = FALSE),
                           selection="single")
    })
    
  })
  
}

getData <- function(pool,tableName,column,value){
  conn <- poolCheckout(pool)
  queryStatus <- dbWithTransaction(conn,{
    query <- paste0("SELECT * FROM ",tableName," WHERE ",column," LIKE '",value,"%'")
    dataframe <-dbGetQuery(conn,query)
  })
  poolReturn(conn)
  return(dataframe)
}

