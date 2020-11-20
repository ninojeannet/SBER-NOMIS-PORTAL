


managementProcessTabUI <- function(id) {
  # Parameters:
  #  - id: String, the module id
  ns <- NS(id)
  
  div(
    div(
      class= 'main-inputs',
      h1("Process data"),
      div(
        class = 'main-actions',
        actionButton(ns('help'), 'Help', class = 'help custom-style custom-style--primary')
      )),
    sidebarLayout(
      sidebarPanel(
        id = ns('sidebar'),
        div(
          selectInput(ns("type"),label = "Select a type",choices = c("dom")),
          actionButton(ns("process"),"Start processing")
        ),
        width=4
      ),
      mainPanel(
        id = ns('main'),
        verbatimTextOutput(ns('status')),
        uiOutput("report"),
        hidden(downloadButton(ns("show"),"Show process result")),
        hidden(downloadButton(ns("save"),"Save results in database"))
        ,
        width=8
      )
    )
  )
}

managementProcessTab <- function(input, output, session,pool){
  
  
  observeEvent(input$process,{
    # rmarkdown::render("processing/dom_processing.Rmd")
    switch (input$type,
      "dom" = {
        print("test")
        output$report <- renderUI({

        }) 
      }
    )
  })
  
  observeEvent(input$show,{
    
  })
  
  observeEvent(input$save,{
    
  })
}


