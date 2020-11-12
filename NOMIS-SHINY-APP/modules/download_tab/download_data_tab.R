
source('./utils/helper_database.R')
source('./utils/helper_dataframe.R')
source('./utils/template_config.R')
source('./utils/dataframe_generator.R')
source('./utils/helper_file.R')
source('./utils/helper_log.R')
source('./modules/data_module/data_validation_popup.R')


# UI function of the downloadDataTab module
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
       hidden(
       verbatimTextOutput(ns('preview'))),
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
  fields <- reactiveVal()
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
    newType <- isolate(input$type)
    if(grepl("All", newType, fixed = TRUE))
      newType <- unlist(unname(list.search(typeList(),newType %in% .)))[-1]
    if(isMultiple()){
      tmp <- c(selectedTypes(),newType)
      selectedTypes(unique(tmp))
      updateSelectizeInput(session,"selectedTypes",choices = selectedTypes())
    }
    else{
      selectedOnlyType(newType[1])
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
    if(isMultiple()){
      fields <- convertFieldNames(selectedTypes())
      df <- generateMergedDownloadDF(fields,ids(),pool)
    }
    else{
      field <- convertFieldNames(selectedOnlyType())
      print(field)
      df <- generateSimpleDownloadDF(field,ids(),pool)
      print(df)
      
    }
    
    dfToDownload(df)
    nbOfGlacier(length(ids()))
    fields(names(df))
    showElement("preview")
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
  
  # Render a small summary of the generated file
  output$preview <- renderPrint({
    isMerged <- TRUE
    cat("# File summary",'\n')
    cat("# Download time :",format(Sys.time(), "%d.%m.%Y %H:%M:%S"),'\n')
    cat('# Number of glacier : ',nbOfGlacier(),'\n')
    cat('# Selected fields','\n',fields(),'\n')
    if(isMerged)
      cat('Some of these data have been average when they have replicates.','\n')
  })
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

# Generate a dataframe of fields from the same table
# the dataframe contains detailed value from the database such as replicates
# Parameters : 
# - field : the field / category of field to put in the data frame
# - ids : lists of glaciers ids
# - pool : the database connection pool
# Return the generated data frame
generateSimpleDownloadDF <- function(field,ids,pool){
  table <- getTableNameFromValue(field)
  fields <- unique(c(mandatoryFields[[table]], getFieldsFromValue(field)))
  df <- getFieldsFromGlacier(pool,table,fields,ids)
  newnames <- c()
  for (field in fields) {
    newName <- str_replace_all(convertColnames(field),"\n"," ")
    newnames <- c(newnames,newName)
  }
  colnames(df) <- newnames
  return(df)
}

# Generate a dataframe which can contains fields from any table.
# The function merge, reduce and scale data to obtains one only data frame containing all the desired parameters
# Parameters : 
# - fields : list of the selected fields. Those fields will be in the dataframe
# - ids : Lists of selecteed glacier ids
# - pool : The database connection pool
# Return the generated data frame
generateMergedDownloadDF <- function(fields,ids,pool){
  tables <- unlist(lapply(fields,getTableNameFromValue))
  print(tables)
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
    print(values)
    column <- scale(values,table,nbEntries)

    if(field=="date")
      column <- format(as.Date(unlist(column)),"%d.%m.%Y")

    if(is.null(ncol(column))){
      name <- str_replace_all(convertColnames(field),"\n"," ")
      df[[name]] <- unlist(column)
      
    }
    else{
      for(i in 1:ncol(column)){
        name <- str_replace_all(convertColnames(names(column[i])),"\n"," ")
        print(column[i])
        df[[name]] <- unlist(column[i])
      }
    }
  }
  print(df)
  return(df)
}

# Get the display name of the given field
# Parameter :
# - field : field to convert 
# Return the converted name
convertColnames <- function(field){
  print(field)
  category <- getCategoryFromValue(field)
  print(category)
  index <- which(templateFieldNames[[category]] == field)[[1]]
  return(fullnameFields[[category]][[index]])
}

# This function merges all the replicate of a parameters to eliminate the replicates 
# and replace it with an average value.
# Parameters : 
# - values : the data frame to reduce 
# - tablename : the table name of the input values
# Return the reduced data frame
reduce <- function(values,tablename){
  
    fk <- tableOptions[[tablename]][["FK"]]
    colToSummarise <-names(values %>% select(-all_of(fk)))
    values <- values %>%                                        
      group_by(.dots=fk) %>%                         
      summarise_at(vars(colToSummarise),mean,na.rm = TRUE) 
    values[is.na(values)] <- NA
    
    if(!is.null(isOnlyUP[[tablename]]) && isOnlyUP[[tablename]]){
      values <- values %>% mutate(rownum = row_number()) %>% 
        bind_rows(., filter(., !is.na(fk)) %>% 
                    mutate(across(colToSummarise,function(.){NA}), rownum = rownum-.5)) %>% 
        arrange(rownum) %>%
        select(-rownum)
    }
    values <- values %>%
        select(-all_of(fk))
  return(values)
}

# This function scale the given data frame to the desired final size.
# It duplicates all values to have a data frame of the right size
# Parameters :
# - values : the data frame to scale
# - tablename : the table name of the data frame 
# - nbFinalEntries : the final number of entries to have per glacier
# Returned the scaled data frame
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