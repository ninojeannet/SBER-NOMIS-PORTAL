source('./utils/helper_database.R')
source('./utils/template_config.R')
source('./utils/helper_download.R')


# UI function of the downloadDataTab module
# Parameters : 
# id : id of the module
downloadFileTabUI <- function(id){
  ns <- NS(id)
  
  div(
    div(
      class= 'main-inputs',
      h1("Download files"),
      div(
        class = 'main-actions',
        actionButton(ns('help'), 'Help', class = 'help custom-style custom-style--primary')
      )),
    sidebarLayout(
      sidebarPanel(
        id = ns('sidebar'),
        div(
          selectInput(ns("type"),label = "Select a data type",choices = downloadFileTypes)
        ),
        div(
          radioButtons(ns("selectRange"), "Choose a selection option :",
                       c("Unique glacier" = "simple",
                         "Range of glacier" = "range",
                         "List of glacier" = "list")),
          textInput(ns("glacier"),"Enter glacier ID"),
          hidden(numericRangeInput(ns("glacierRange"),label = "Glacier range", value = c(1, 500))),
          hidden(textInput(ns("glacierList"),"Glacier list (comma separated)")),
          actionButton(ns("generate"),"Select files")
        ),
        width=4
      ),
      mainPanel(
        id = ns('main'),
        verbatimTextOutput(ns('preview')),
        hidden(downloadButton(ns("downloadFile"),"Download files"))
        ,
        width=8
      )
    )
  )
}

# Server function of the downloadDataTab module
# Parameters : 
# input : input of the shiny app
# output : output of the shiny app
# session : session of the shiny app
# pool : connection pool to access the database
downloadFileTab <- function(input,output,session,pool){

  files <- reactiveVal()
  # Reactive variable
  tableName <- reactive(getTableNameFromValue({input$type}))
  selectedFields <- reactive(getFieldsFromValue(input$type))
  
  # Reactive variable which contains the list of all chosen glacier's id
  ids <- reactive({
    switch (input$selectRange,
            "simple" = {
              ids <- input$glacier
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
            },
            "range" = {
              hideElement("glacier")
              showElement("glacierRange")
              hideElement("glacierList")
            },
            "list" = {
              hideElement("glacier")
              hideElement("glacierRange")
              showElement("glacierList")
            }
    )
  })
  
  # ObserveEvent that reacts to the "generate" input button
  # Generate a file according to the given parameters and glacier ids
  observeEvent(input$generate,{
    # Render a small summary of the generated file
    print(tableName())
    print(selectedFields())
    output$preview <- renderPrint({
      validateInput(input)
      files(getExistingFilenamesInDB(pool,tableName(),selectedFields(),ids()))
      cat("# Download summary",'\n')
      cat("# Download time :",format(Sys.time(), "%d.%m.%Y %H:%M:%S"),'\n')
      cat('# Files : ','\n')
      for (file in files()) {
        cat('\t',file,'\n')
      }
    })

    showElement("downloadFile")
  })
  
  # Handler that allow user to download the previously generated file
  output$downloadFile <- downloadHandler(
    filename = function() {
      paste("nomis-",format(Sys.time(), "%Y%m%d-%H%M"),'-',selectedFields(), ".zip", sep = "")
    },
    content = function(file) {
      fs <- c()
      for (filename in files()) {
        path <- getFilePath(selectedFields(),filename)
        fs <- c(fs, path)
      }
      zip(zipfile=file, files=fs)
    },
    contentType = "application/zip"
  )
}


# Check if the input format are valid and if not display a message
# Parameters :
# input : input of the shiny app
validateInput <- function(input){
  shiny::validate(
    switch (isolate(input$selectRange),
            "simple" = {need(grep('GL(\\d){1,3}',isolate(input$glacier)), 'Please insert a valid ID ( ex : GL23 )')},
            "range" = {
              range <- isolate(input$glacierRange)
              need(range[2] > range[1] & range[2] > 0 & range[1] > 0, 'Please insert a valid range of id')},
            "list" = {need(grep('^[0-9]+(,[0-9]+)*$',isolate(input$glacierList)), 'Please insert a valid list of ID ( ex : 23,45,56 )')}
    )
  )
}