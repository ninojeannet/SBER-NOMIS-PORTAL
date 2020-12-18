
source('./utils/helper_database.R')
source('./utils/template_config.R')
source('./utils/helper_download.R')


# UI function of the downloadDataTab module
# Parameters : 
# id : id of the module
downloadDataTabUI <- function(id){
  ns <- NS(id)
  
  div(
    div(
      class= 'main-inputs',
      h1("Download data"),
      div(
        class = 'main-actions',
        actionButton(ns('help'), 'Help', class = 'help custom-style custom-style--primary')
      )),
    sidebarLayout(
      sidebarPanel(
        id = ns('sidebar'),
        div(
          prettySwitch(ns("multiple"),"Allow multiple parameters selection",value = TRUE,fill = TRUE),
          div(class="lists",
          div(class="list",selectInput(ns('type'), 'Type', choices= typeList(), multiple=TRUE, selectize=FALSE)),
          div(class="act-btn",div(class="bloc",actionButton(ns("add"),">>")),
              div(class="bloc",actionButton(ns("remove"),"X"))),
          hidden(div(id=ns("m"),class="list",selectInput(ns('selectedTypes'),choices= c(), 'Selected types', multiple=TRUE, selectize=FALSE))),
          div(id=ns("o"),class="list",textInput(ns('onlyType'), 'Selected type')))
        ),
        div(
          radioButtons(ns("selectRange"), "Choose a selection option :",
                       c("Unique glacier" = "simple",
                         "Range of glacier" = "range",
                         "List of glacier" = "list")),
          textInput(ns("glacier"),"Enter glacier ID"),
          hidden(numericRangeInput(ns("glacierRange"),label = "Glacier range", value = c(1, 500))),
          hidden(textInput(ns("glacierList"),"Glacier list (comma separated)")),
          actionButton(ns("generate"),"Generate download files")
          ),
        width=7
      ),
     mainPanel(
       id = ns('main'),
      
       verbatimTextOutput(ns('preview')),
       hidden(downloadButton(ns("downloadFile"),"Download file"))
       ,
       width=5
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
downloadDataTab <- function(input,output,session,pool){
  # Reactive values
  selectedTypes <- reactiveVal()
  selectedOnlyType <- reactiveVal()
  dfToDownload <- reactiveVal()
  nbOfEntry <- reactiveVal()
  nbOfGlacier <- reactiveVal()
  isMultiple <- reactiveVal(FALSE)
  
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
  
  # ObserveEvent that reacts to "multiple" input update
  # toggle multiple / single parameters selection
  observeEvent(input$multiple,{
    disable("onlyType")
    toggle("m")
    toggle("o")
    isMultiple(!isMultiple())
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
  
  # ObserveEvent that reacts to "add" input button
  # Add selected parameters from params list to the selected parameters input 
  observeEvent(input$add,{
    newTypes <- isolate(input$type)
    types <- vector()
    for (newType in newTypes) {
      if(grepl("All", newType, fixed = TRUE))
        types <- c(types,unlist(unname(list.search(typeList(),newType %in% .)))[-1])
      else
        types <- c(types,newType)
    }
    
    if(isMultiple()){
      tmp <- c(selectedTypes(),types)
      selectedTypes(unique(tmp))
      updateSelectizeInput(session,"selectedTypes",choices = selectedTypes())
    }
    else{
      selectedOnlyType(types[1])
      updateTextInput(session,"onlyType",value = selectedOnlyType())
    }
  })
  
  # ObserveEvent that reacts to "remove" input button
  # remove selected parameters from selected params list 
  observeEvent(input$remove,{
    typesToRemove <- isolate(input$selectedTypes)
    if(isMultiple()){
      tmp <- setdiff(selectedTypes(),typesToRemove)
      selectedTypes(unique(tmp))
      updateSelectizeInput(session,"selectedTypes",choices = selectedTypes())
    }
    else{
      selectedOnlyType("")
      updateTextInput(session,"onlyType",value = selectedOnlyType())
    }
  })
  
  # ObserveEvent that reacts to the "generate" input button
  # Generate a file according to the given parameters and glacier ids
  observeEvent(input$generate,{
    # Render a small summary of the generated file
    # output$preview <- renderPrint({
    #   validateDownloadInput(input,isMultiple(),isolate(selectedTypes()))

      if(isMultiple()){
        fields <- convertFieldNames(isolate(selectedTypes()))
        df <- generateMergedDownloadDF(fields,ids(),pool)
      }
      else{
        field <- convertFieldNames(isolate(selectedOnlyType()))
        df <- generateSimpleDownloadDF(field,ids(),pool)
      }
      
      dfToDownload(df)
    #   nbOfGlacier(length(ids()))
    #   nbOfEntry(nrow(df))
    #   isMerged <- TRUE
    #   cat("# File summary",'\n')
    #   cat("# Download time :",format(Sys.time(), "%d.%m.%Y %H:%M:%S"),'\n')
    #   cat('# Number of glacier : ',nbOfGlacier(),'\n')
    #   cat('# Total number of rows : ',nbOfEntry(),'\n')
    #   if(isMultiple())
    #     cat('Some of these data have been average when they have replicates.','\n')
    # })

    showElement("downloadFile")
  })
  
  # Handler that allow user to download the previously generated file
  output$downloadFile <- downloadHandler(
    filename = function() {
      paste("nomis-",format(Sys.time(), "%Y%m%d-%H%M"),'-db', ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dfToDownload(), file, row.names = FALSE,na = "NA")
    }
  )
  
}

# Check if the input format are valid and if not display a message
# Parameters :
# - input : input of the shiny app
# - isMultiple : boolean. Indicate whether it's a multiple parameters selection
# - selectedTypes : give the list of selected parameters
validateDownloadInput <- function(input,isMultiple,selectedTypes){
  shiny::validate(
    if(isMultiple)
      need(length(selectedTypes) != 0L, 'Please select at least one parameter')
    else
      need(isolate(input$onlyType), 'Please select a parameter'),
    
    switch (isolate(input$selectRange),
            "simple" = {need(grep('GL(\\d){1,3}',isolate(input$glacier)), 'Please insert a valid ID ( ex : GL23 )')},
            "range" = {
              range <- isolate(input$glacierRange)
              need(range[2] > range[1] & range[2] > 0 & range[1] > 0, 'Please insert a valid range of id')},
            "list" = {need(grep('^[0-9]+(,[0-9]+)*$',isolate(input$glacierList)), 'Please insert a valid list of ID ( ex : 23,45,56 )')}
    )
  )
}

# Convert a list of displayed field names to their database names
# Parameters : 
# - fieldstoConvert : the list of names to convert
# Return the converted list
convertFieldNames <- function(fieldstoConvert){
  convertedFields <- c()
  for (field in fieldstoConvert) {
    l <- list.search(downloadDataTypes,field %in% names(.))[[1]]
    value <- l[[field]]
    convertedFields <- c(convertedFields,value)
  }
  return(convertedFields)
}