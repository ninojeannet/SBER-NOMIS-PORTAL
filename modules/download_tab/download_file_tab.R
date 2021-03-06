source('./utils/helper_database.R')
source('./utils/template_config.R')
source('./utils/helper_download.R')


# UI function of the downloadDataTab module
# Parameters : 
# id : id of the module
downloadFileTabUI <- function(id,pool){
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
          selectInput(ns("type"),label = "Select a data type",choices = downloadFileTypes),
          hidden(selectInput(ns("expedition"),label = "Select an expedition",choices = c()))
        ),
        div(id=ns("glacierSelection"),
          radioButtons(ns("selectRange"), "Choose a selection option :",
                       c("Unique glacier" = "simple",
                         "Range of glacier" = "range",
                         "List of glacier" = "list",
                         "Expedition"="expedition")),
          textInput(ns("glacier"),"Enter glacier ID"),
          hidden(numericRangeInput(ns("glacierRange"),label = "Glacier range", value = c(MIN,MAX))),
          hidden(textInput(ns("glacierList"),"Glacier list (comma separated)")),
          hidden(selectInput(ns("expedSelection"),label = "Select an expedition",choices = formatExpedList(pool)))
        ),actionButton(ns("generate"),"Select files"),
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
  
  # observeEvent that react to type input's update
  # Hide / show html elements
  observeEvent(input$type,{
    
    if(input$type %in% uploadDNAFilesType || input$type == "trace_el" ||input$type == "ft_icr_ms" ){
      exped <-getTable("expedition",pool)
      exped <- setNames(as.character(exped$id_expedition), exped$name)
      updateSelectInput(session,"expedition",label = "Select an expedition",choices = exped)
      hideElement("glacierSelection")
      showElement("expedition")
    }
    else {
      showElement("glacierSelection")
      hideElement("expedition")
    }
  },ignoreInit = TRUE)
  
  # ObserveEvent that reacts to the "generate" input button
  # Generate a file according to the given parameters and glacier ids
  observeEvent(input$generate,{
    # Render a small summary of the generated file
    output$preview <- renderPrint({
      if(!(input$type %in% uploadDNAFilesType) && input$type != "trace_el" && input$type != "ft_icr_ms" ){
        validateInput(input)
        files(getExistingFilenamesInDB(pool,tableName(),selectedFields(),ids()))
      }
      else{
        files(getExistingExpedFilenameInDB(pool,input$expedition,input$type))
      }
      if(is.character(files())){
        cat("# Download summary",'\n')
        cat("# Download time :",format(Sys.time(), "%d.%m.%Y %H:%M:%S"),'\n')
        cat('# Files : ','\n')
        for (file in files()) {
          cat('\t',file,'\n')
        }
        showElement("downloadFile")
      }
      else{
        cat("# No file available for these criteria.",'\n')
        hideElement("downloadFile")
      }

    })

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
  
  # Create an observeEvent that react to the help button
  observeEvent(input$help, {
    # Create modal with the corresponding htmlOutput
    title <- 'Download data help'
    template <-'./html_components/help_download_file.html'
    
    showModal(modalDialog(
      title = title,
      htmlTemplate(template),
      footer = modalButtonWithClass('Dismiss', class = 'custom-style'),
      easyClose = TRUE
    ))
  })
}


# Check if the input format are valid and if not display a message
# Parameters :
# input : input of the shiny app
validateInput <- function(input){
  shiny::validate(
    switch (isolate(input$selectRange),
            "simple" = {need(grep('(\\d){1,3}',isolate(input$glacier)), 'Please insert a valid ID ( ex : 23 )')},
            "range" = {
              range <- isolate(input$glacierRange)
              need(range[2] > range[1] & range[1] > MIN-1 & range[2] < MAX+1, paste0('Please insert a valid range of id (between ',MIN,' and ',MAX,')'))},
            "list" = {need(grep('^[0-9]+(,[0-9]+)*$',isolate(input$glacierList)), 'Please insert a valid list of ID ( ex : 23,45,56 )')}
    )
  )
}