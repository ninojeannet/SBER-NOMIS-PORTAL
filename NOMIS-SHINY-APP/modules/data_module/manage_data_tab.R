
source('./utils/helper_database.R')
source('./utils/helper_dataframe.R')
source('./utils/template_config.R')
source('./utils/dataframe_generator.R')
source('./utils/helper_file.R')
source('./utils/helper_log.R')

# UI function of the uploadDataTab module
# Parameters : 
# id : id of the module
manageDataTabUI <- function(id){
  ns <- NS(id)
  
  # Create the sidebarLayout
  sidebarLayout(
    # Create a sidebar with the innerModule first unit input UI elements inside
    sidebarPanel(
      id = ns('sidebar'),
      div(
        selectInput(ns("type"),label = "Select a data type",choices = uploadDataTypes),
        hidden(selectInput(ns("domtype"),label = "Select a DOM parameter",choices = uploadDOMTypes)),
        radioButtons(ns("selectRange"), "Choose a selection option :",
                     c("Unique glacier" = "simple",
                       "Range of glacier" = "range",
                       "List of glacier" = "list")),
        textInput(ns("glacier"),"Enter glacier ID"),
        hidden(numericRangeInput(ns("glacierRange"),label = "Glacier range", value = c(1, 500))),
        hidden(textInput(ns("glacierList"),"Glacier list (comma separated)")),
        actionButton(ns("generate"),"Generate template"),
        hidden( fileInput(ns("files"),"Select files",accept=".csv",multiple = TRUE))
      ),
      width = 4
    ),
    # Create the main panel with the innerModule first unit plot UI elements inside
    mainPanel(
      id = ns('main'),
      div(
        hidden(h1(id = ns("title"),"Selected files status")),
        div(class = "file-output",id=ns("tables"),
            div(class="file-table",tableOutput(ns("fileValid"))),
            div(class="file-table",tableOutput(ns("fileWrong"))),
            div(class="file-table",tableOutput(ns("fileMissing"))),
            div(class="file-table",hidden(actionButton(ns("uploadFiles"),"Upload valid files",class="upload-button")))
        ),
        rHandsontableOutput(ns("table")),
        hidden(actionButton(ns("upload"),"Upload data",class="upload-button"))       
      ),
      width = 8
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
manageDataTab <- function(input,output,session,pool,dimension,isUploadOnly){
  
  tableName <- reactive(getTableNameFromValue({input$type}))
  selectType <- reactive({input$selectRange})
  
  dataf <- reactive({
    if(input$type %in% names(templateFieldNames))
      tmp <- getTableFromGlacier(pool,tableName(),ids())
    else
      tmp <- getFieldFromGlacier(pool,tableName(),input$type,ids())
    
    return(generateFilledDF(tmp,tableName(),ids()))
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
  
  observeEvent(input$generate,{
    output$table <- renderRHandsontable({
      validateInput(input)
      table <- isolate(tableName())
      df <- isolate(dataf())
      if(isUploadOnly)
        readOnlyRows <- as.numeric(getReadOnlyRows(df,isolate(tableName())))
      else
        readOnlyRows <- vector()
      rhandsontable(df,digits=10,overflow='visible',stretchH = "all", height = dimension()[2]/100*70)%>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)%>%
        hot_cols(format = tableOptions[[table]][["format"]]) %>%
        hot_col(mandatoryFields[[table]], readOnly = TRUE) %>%
        hot_row(readOnlyRows, readOnly = TRUE)
    })
    showElement("upload")
    showElement("table")
  })
  
  observeEvent(input$type,{
    if (input$type == "biogeo" & input$domtype %in% c("eem","abs1","abs10")){
      showElement("domtype")
      showElement("files")
      hideElement("generate")
      hideElement("table")
      hideElement("upload")
    }
    else{
      hideElement("domtype")
      hideElement("files")
      hideElement("title")
      hideElement("uploadFiles")
      hideElement("tables")
      showElement("generate")
    }
  })
  
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
            }
    )
  })
  
  filenames <- reactive({generateFilenames(ids(),input$domtype)})
  tablesFile <- reactive({generateFileTables(filenames(),input$files)})
  
  observeEvent(input$files,{
    showElement("uploadFiles")
    showElement("title")
    showElement("tables")
    
    tables <- tablesFile()
    
    output$fileValid <- renderTable({ 
      validateInput(input)
      data.frame("Valid" =tables[["valid"]])})
    output$fileWrong <- renderTable({ 
      validateInputEmpty(input)
      data.frame("Wrong" =tables[["wrong"]])})
    output$fileMissing <- renderTable({ 
      validateInputEmpty(input)
      data.frame("Missing" =tables[["missing"]])})
    
    if(nrow(data.frame("Valid" =tables[["valid"]])) > 0)
      enable("uploadFiles")
    else
      disable("uploadFiles")
  })
  
  observeEvent(input$upload,{
    out <- hot_to_r(input$table)
    print(out)
    status <- saveData(out,isolate(tableName()),pool)
    if (status)
      saveLog("upload","Nino",paste0("Upload data ",isolate(tableName())," in the database"))
    else
      saveLog("upload","Nino",paste0("FAILED Upload data ",isolate(tableName())," in the database"))
  })
  
  observeEvent(input$uploadFiles,{
    tryCatch({
      withProgress(message = "Saving valid files",value = 0,{
        
        validFilename <- tablesFile()[["valid"]]
        validFiles <- input$files[input$files$name %in% validFilename,]
        print(validFiles)
        nbOfFiles <- nrow(validFiles)
        for (row in 1:nrow(validFiles)) {
          name <- validFiles[row,"name"]
          saveFile(name,validFiles[row,"datapath"],input$domtype)
          pkValue <- str_remove(name,"_[^_]+\\..+")
          fkValue <- str_remove(name,"_[^_]+_[^_]+\\..+")
          replicate <- str_extract(str_extract(name,"_\\d_"),"\\d")
          saveFieldInDB(tableName(),paste0("filename_",input$domtype),pkValue,fkValue,replicate,name,pool)
          incProgress(1/nbOfFiles, detail = paste("Saving ", name))
        }
      })
      showNotification("Files successfully inserted into database",type = "message")
    },
    warning = function(war){
      print(war)
      showNotification(war$message, type = "warning")
    },
    error = function(err){
      print(err)
      showNotification(err$message,type = "error",duration = NULL)
    },
    finally = function(f){
      print(f) 
    })
  })
}


# Check for rows with existing data in the database and return a list of index of these rows
# Parameters : 
# - dataframe : the dataframe to check for not empty rows
# - tablename : the name of the table of the current dataframe
# Return a list of row indexes of not empty rows
getReadOnlyRows <- function(dataframe,tablename){
  colNames <- setdiff(colnames(dataframe),unlist(mandatoryFields[tablename]))
  dataframe <- dataframe[colNames]
  onlyExistingRows <- dataframe[rowSums(is.na(dataframe)) != ncol(dataframe),,drop = FALSE]
  rows <- rownames(onlyExistingRows)
  return(rows)
}

# Return the name of the table containing a given field
# Check in the config file the tablename
# Parameters :
# - value : name of the field to find its table for
# return the name of the table
getTableNameFromValue <- function(value){
  l <- list.search(templateFieldNames,value %in% .)
  if(length(l) >0)
    tablename <- names(l)[1]
  else
    tablename <- value
  print(tablename)
  
  return(tablename)
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


