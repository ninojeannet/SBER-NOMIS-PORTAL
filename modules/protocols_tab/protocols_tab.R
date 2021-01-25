

protocolsTabUI <- function(id) {
  # Parameters:
  #  - id: String, the module id
  ns <- NS(id)
  
  div(
    div(
      class= 'main-inputs',
      h1("Protocols"),
      div(
        class = 'main-actions',
        actionButton(ns('help'), 'Help', class = 'help custom-style custom-style--primary'),
        actionButton(ns('refresh'), 'Refresh', class = 'help custom-style custom-style--primary')

      )),
    # Create the sidebarLayout
    sidebarLayout(
      sidebarPanel(
        id = ns('sidebar'),
        div(
          uiOutput(ns("container"))
        ),
        width = 3
      ),
      # Create the main panel
      mainPanel(
        id = ns('main'),
        div(id="main-content",
            uiOutput(ns("protocoloutput")),
            
        ),
        width = 9
      )
    )
  )
}

protocolsTab <- function(input, output, session,pool){
  refresh <- reactiveVal(TRUE)
  
  protocols <- reactive({
    refresh()
    table <-getTable("protocol",pool)
    values <- setNames(as.character(table$filename), table$name)
  })
  
  observeEvent(input$refresh,{
    refresh(!refresh())
  })
  
  output$container <- renderUI({
    selectInput(session$ns("param"),label = "Select a parameter",choices = c("-",protocols()))
  })
  
  output$protocoloutput <- renderUI({
    if(!is.null(input$param) && input$param!="-" )
      tags$iframe(style="height:600px; width:100%", src=paste0("protocols/",input$param))
  })
}
