source('./utils/helper_database.R')
source('./utils/helper_dataframe.R')
source('./utils/template_config.R')
source('./utils/dataframe_generator.R')
source('./utils/helper_file.R')
source('./utils/helper_log.R')
source('./utils/helper_visualisation.R')

source('./modules/data_module/data_validation_popup.R')

visualisationVersusTabUI <- function(id,pool) {
  # Parameters:
  #  - id: String, the module id
  ns <- NS(id)
  
  div(
    div(
      class= 'main-inputs',
      h1("Visualisation"),
      div(
        class = 'main-actions',
        actionButton(ns('help'), 'Help', class = 'help custom-style custom-style--primary')
      )),
    # Create the sidebarLayout
    sidebarLayout(
      sidebarPanel(
        id = ns('sidebar'),
        div(
          selectInput(ns("paramX"),label = "Select a parameter for X axis",choices = plotDataTypes),
          selectInput(ns("paramY"),label = "Select a parameter for Y axis",choices = plotDataTypes),
          radioButtons(ns("selectRange"), "Choose a selection option :",
                       c("Unique glacier" = "simple",
                         "Range of glaciers" = "range",
                         "List of glaciers" = "list",
                         "Expedition"="expedition")),
          textInput(ns("glacier"),"Enter glacier ID"),
          hidden(numericRangeInput(ns("glacierRange"),label = "Range of glaciers", value = c(MIN,MAX))),
          hidden(textInput(ns("glacierList"),"List of glaciers (comma separated)")),
          hidden(selectInput(ns("expedSelection"),label = "Select an expedition",choices = formatExpedList(pool))),
          actionButton(ns("generate"),"Generate plot"),
        ),
        width = 3
      ),
      # Create the main panel
      mainPanel(
        id = ns('main'),
        div(
            plotOutput(ns("plot"))       
        ),
        width = 9
      )
    )
  )
}

visualisationVersusTab <- function(input, output, session,pool){
  
  fields <- reactive({
    c(input$paramX,input$paramY)
  })
  
  # Reactive variable which contains the list of all chosen glacier's id
  ids <- reactive({
    switch (input$selectRange,
            "simple" = {
              ids <- paste0("GL",input$glacier)
            },
            "range" = {
              range <- input$glacierRange
              ids <- c(paste0("GL",as.character(range[1]:range[2])))
            },
            "list" = {
              ids <- input$glacierList
              ids <- gsub(" ", "", ids, fixed = TRUE)
              ids <- strsplit(ids,',')
              ids <- sapply(ids, function(x){paste0("GL",x)})
            },
            "expedition" ={
              ranges <- getTable("glacier_range",pool) %>% filter(id_expedition == input$expedSelection)
              ids <- vector()
              for (i in 1:nrow(ranges)) {
                ids <- c(ids,paste0("GL",as.character(ranges[[i,"min"]]:ranges[[i,"max"]])))
              }
              ids
            })
  })
  
  # observeEvent that react to selectrange input's update
  # Enable / disable html component 
  observeEvent(input$selectRange,{
    switch (input$selectRange,
            "simple" = {
              showElement("glacier")
              hideElement("glacierRange")
              hideElement("glacierList")
              hideElement("expedSelection")
            },
            "range" = {
              hideElement("glacier")
              showElement("glacierRange")
              hideElement("glacierList")
              hideElement("expedSelection")
            },
            "list" = {
              hideElement("glacier")
              hideElement("glacierRange")
              showElement("glacierList")
              hideElement("expedSelection")
            },
            "expedition" = {
              hideElement("glacier")
              hideElement("glacierRange")
              hideElement("glacierList")
              showElement("expedSelection")
            }
    )
  })
  
  data <- reactive({
    generateMergedDF(fields(),ids(),pool)
  })
  
  observeEvent(input$generate,{
    output$plot <- renderPlot({
      validateVersus(input)
      validateInput(input)
      df <- data()
      print(df)
      nbCol <- ncol(df)
      VSPlot(df,colnames(df[2]),colnames(df[3]),colnames(df[nbCol-1]),colnames(df[nbCol]))
      })
  })
  

  
}

validateVersus <- function(input){
  shiny::validate(need(input$paramX != input$paramY, 'You need to choose different parameters'))
}
