

managementMainUI <- function(id) {
  # Create the UI for the downloadTab module
  # Parameters:
  #  - id: String, the module id
  ns <- NS(id)
  div(
    selectInput(ns("tmp"),label="TT",choices = "choic"),
    dataTableOutput(ns("outputTable")),
    actionButton(ns("search"),label = "search")
    
    
  )
  # renderText(ns("tmp"),{"test"})
  
}

managementMain <- function(input, output, session,vars,pool){
  
  
  observeEvent(input$search,{
    output$outputTable <- DT::renderDataTable({
      data <- getData(pool)
      table<-DT::datatable(data,rownames = FALSE,
        options = list(searching = FALSE, lengthChange = FALSE),
        selection="single")
    })

  })

}

getData <- function(pool){
  conn <- poolCheckout(pool)
  queryStatus <- dbWithTransaction(conn,{
    dataframe <-dbGetQuery(conn,"SELECT * FROM glacier")
  })
  poolReturn(conn)
  return(dataframe)
}