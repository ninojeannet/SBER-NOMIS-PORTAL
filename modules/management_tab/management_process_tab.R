


managementProcessTabUI <- function(id) {
  # Parameters:
  #  - id: String, the module id
  ns <- NS(id)
  
  div("Functionality not available yet..."
    # div(
    #   class= 'main-inputs',
    #   h1("Process data"),
    #   div(
    #     class = 'main-actions',
    #     actionButton(ns('help'), 'Help', class = 'help custom-style custom-style--primary')
    #   )),
    # sidebarLayout(
    #   sidebarPanel(
    #     id = ns('sidebar'),
    #     div(
    #       selectInput(ns("type"),label = "Select a type",choices = c("dom")),
    #       actionButton(ns("process"),"Start processing")
    #     ),
    #     width=4
    #   ),
    #   mainPanel(
    #     id = ns('main'),
    #     verbatimTextOutput(ns('status')),
    #     uiOutput("report"),
    #     actionButton(ns("show"),"Show process result"),
    #     hidden(actionButton(ns("save"),"Save results in database"))
    #     ,
    #     width=8
    #   )
    # )
  )
}

managementProcessTab <- function(input, output, session,pool){
  
  
  observeEvent(input$process,{
   
    switch (input$type,
      "dom" = {
        rmarkdown::render("processing/dom_processing.Rmd",output_file = "dom_processing.html",output_dir = "processing/tmp/")
      }
    )
  })
  
  observeEvent(input$show,{
    showModal(
      modalDialog(
                includeHTML("processing/tmp/dom_processing.html"),
                easyClose=TRUE,
                
            
        )
      )
    
  })
  
  observeEvent(input$save,{
    
  })
}


