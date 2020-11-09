
source('./utils/helper_database.R')
source('./utils/helper_dataframe.R')
source('./utils/template_config.R')
source('./utils/dataframe_generator.R')
source('./utils/helper_file.R')
source('./utils/helper_log.R')
source('./modules/data_module/data_validation_popup.R')


# UI function of the uploadDataTab module
# Parameters : 
# id : id of the module
downloadDataTabUI <- function(id){
  ns <- NS(id)
  
  div(
    div(
      class= 'main-inputs',
      div(
        class = 'main-actions',
        actionButton(ns('help'), 'Help', class = 'help custom-style custom-style--primary')
      )),
    sidebarLayout(
      # Create a sidebar with the innerModule first unit input UI elements inside
      sidebarPanel(
        id = ns('sidebar'),
        div(
          
          div(class="inline list",selectInput(ns('type'), 'Type', choices= typeList(), multiple=TRUE, selectize=FALSE)),
          div(class="inline act-btn",div(class="bloc",actionButton(ns("add"),">>")),
              div(class="bloc",actionButton(ns("remove"),"X"))),
          div(class="inline list",selectInput(ns('selectedTypes'),choices= c(), 'Selected types', multiple=TRUE, selectize=FALSE))
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
       downloadButton(ns("downloadFile"),"Download file")
       ,
       width=5
       )
  )
  )
}



# Server function of the uploadDataTab module
# Parameters : 
# input : input of the shiny app
# output : output of the shiny app
# session : session of the shiny app
# pool : connection pool to access the database
# dimension : window size
downloadDataTab <- function(input,output,session,pool){
  # Create a boolean reactive value that keep track of the sidebar visibility state

  tableName <- reactive(getTableNameFromValue({input$type}))
  selectedFields <- reactive(getFieldsFromValue(input$type))
  selectedTypes <- reactiveVal()
  
  dfToDownload <- reactiveVal()
  
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
  
  observeEvent(input$add,{
    newType <- isolate(input$type)
    tmp <- c(selectedTypes(),newType)
    selectedTypes(unique(tmp))
    updateSelectizeInput(session,"selectedTypes",choices = selectedTypes())
  })
  
  observeEvent(input$remove,{
    typesToRemove <- isolate(input$selectedTypes)
    tmp <- setdiff(selectedTypes(),typesToRemove)
    selectedTypes(unique(tmp))
    updateSelectizeInput(session,"selectedTypes",choices = selectedTypes())
  })
  
  observeEvent(input$generate,{
    fields <- convertFieldNames(selectedTypes())
    df <- generateDownloadDF(fields,ids(),pool)
    dfToDownload(df)
  })
  
  output$downloadFile <- downloadHandler(
    filename = function() {
      paste("test", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dfToDownload(), file, row.names = FALSE,na = "NA")
    }
  )
}

convertFieldNames <- function(fieldstoConvert){
  convertedFields <- c()
  for (field in fieldstoConvert) {
    l <- list.search(downloadDataTypes,field %in% names(.))[[1]]
    value <- l[[field]]
    convertedFields <- c(convertedFields,value)
  }
  return(convertedFields)
}

generateDownloadDF <- function(fields,ids,pool){
  tables <- unlist(lapply(fields,getTableNameFromValue))
  nbEntries <- max(levels[tables])
  nbRow <- nbEntries*length(ids)
  df <- data.frame(matrix(ncol = 0, nrow = nbRow))
  if(nbEntries == 6)
    df[["patch"]] <- unlist(getFieldsFromGlacier(pool,tableName = "patch" ,fields = c("id_patch"),ids = ids))
  else if(nbEntries == 2)
    df[["location"]] <- unlist(getFieldsFromGlacier(pool,tableName = "location" ,fields = c("id_location"),ids = ids))
  else
    df[["glacier"]] <-  unlist(ids)
  
  for (field in fields) {
    table <- getTableNameFromValue(field)
    fieldsToRetrieve <- setdiff(getFieldsFromValue(field),mandatoryFields[[table]])
    nbReplicates <- length(tableOptions[[table]][["replicates"]])
    
    if(nbReplicates > 1){
      values <-getFieldsWithFKFromGlacier(pool,tablename = table ,fields = fieldsToRetrieve,ids = ids)
      values <- reduce(values,table)
    }
    else
    {
      values <-getFieldsFromGlacier(pool,tableName = table ,fields = fieldsToRetrieve,ids = ids)
    }
    column <- unlist(scale(values,table,nbEntries))

    if(field=="date")
      column <- format(as.Date(column),"%d.%m.%Y")
    df[[field]] <- column
  }
  print(df)
  return(df)
}

reduce <- function(values,tablename){
    fk <- tableOptions[[tablename]][["FK"]]
    colToSummarise <-names(values %>% select(-all_of(fk)))
    values <- values %>%                                        
      group_by(.dots=fk) %>%                         
      summarise_at(vars(colToSummarise),mean) %>%  
      select(-all_of(fk))
  return(values)
}

scale <- function(values,tablename,nbFinalEntries){

  nbEntries <- levels[tablename]
  factor <- nbFinalEntries / nbEntries
  newdf <- values[rep(row.names(values), each=factor), 1:ncol(values) ]
  newdf <- as.data.frame(newdf)
  colnames(newdf) <- names(values)
  return(newdf)
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

validateInputEmpty <- function(input){
  shiny::validate(
    switch (isolate(input$selectRange),
            "simple" = {need(grep('GL(\\d){1,3}',isolate(input$glacier)), '')},
            "range" = {
              range <- isolate(input$glacierRange)
              need(range[2] > range[1] & range[2] > 0 & range[1] > 0, '')},
            "list" = {need(grep('^[0-9]+(,[0-9]+)*$',isolate(input$glacierList)), '')}
    )
  )
}