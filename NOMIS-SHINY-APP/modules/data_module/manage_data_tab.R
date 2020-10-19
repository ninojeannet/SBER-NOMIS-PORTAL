
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
manageDataTabUI <- function(id){
  ns <- NS(id)
  
  div(
    # div(class="btn-menu",actionButton(ns('toggleSidebar'), 'Hide inputs', class = 'custom-style')),
    
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
        width = 3
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
              div(class="file-table",tableOutput(ns("fileExisting"))),
              div(class="file-table",hidden(actionButton(ns("uploadFiles"),"Upload valid files",class="upload-button")))
          ),
          rHandsontableOutput(ns("table")),
          hidden(actionButton(ns("upload"),"Upload data",class="upload-button"))       
        ),
        width = 9
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
manageDataTab <- function(input,output,session,pool,dimension,isUploadOnly){
  # Create a boolean reactive value that keep track of the sidebar visibility state
  sidebarVisible <- reactiveVal(TRUE)
  updatedRows <- reactiveVal(vector())
  isFileUpload <- reactiveVal(FALSE)
  
  tableName <- reactive(getTableNameFromValue({input$type}))
  selectType <- reactive({input$selectRange})
  filenames <- reactive({generateFilenames(ids(),input$domtype)})
  existingFiles <- reactive({getExistingFilenamesInDB(pool,tableName(),input$domtype,ids())})
  tablesFile <- reactive({generateFileTables(filenames(),input$files,existingFiles(),isUploadOnly)})
  
  dataf <- reactive({
    if(input$type %in% names(templateFieldNames))
      tmp <- getTableFromGlacier(pool,tableName(),ids())
    else
      tmp <- getFieldFromGlacier(pool,tableName(),input$type,ids())
    
    return(generateFilledDF(tmp,tableName(),ids()))
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
  
  observeEvent(input$generate,{
    output$table <- renderRHandsontable({
      validateInput(input)
      table <- isolate(tableName())
      df <- isolate(dataf())
      if(isUploadOnly)
        readOnlyRows <- as.numeric(getReadOnlyRows(df,isolate(tableName())))
      else
        readOnlyRows <- vector()
      generateHandsonTable(df,dimension,readOnlyRows,table)
    })
    showElement("upload")
    showElement("table")
  })
  
  observeEvent(input$type,{
    if (input$type == "biogeo" & input$domtype %in% c("eem","abs1","abs10")){
      isFileUpload(TRUE)
      showElement("domtype")
      showElement("files")
      hideElement("generate")
      hideElement("table")
      hideElement("upload")
    }
    else{
      isFileUpload(FALSE)
      hideElement("domtype")
      hideElement("files")
      hideElement("title")
      hideElement("uploadFiles")
      hideElement("tables")
      showElement("generate")
    }
  })
  
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
    output$fileExisting <- renderTable({ 
      validateInputEmpty(input)
      data.frame("Existing" =tables[["existing"]])})
    
    if(nrow(data.frame("Valid" =tables[["valid"]])) > 0)
      enable("uploadFiles")
    else
      disable("uploadFiles")
  })
  
  observeEvent(input$upload,{
    out <- hot_to_r(input$table)
    if(!isUploadOnly)
      show_validation_popup(tableName(),updatedRows(),output,session$ns,FALSE,out)
    else
      uploadData(out,isolate(tableName()),pool)
  })
  
  observeEvent(input$uploadFiles,{
    validFilename <- tablesFile()[["valid"]]
    validFiles <- input$files[input$files$name %in% validFilename,]
    if(!isUploadOnly)
      show_validation_popup(tableName(),validFilename,output,session$ns,TRUE)
    else
      processFiles(validFilename,validFiles,tableName(),input$domtype,pool)
  })
  
  observeEvent(input$submit, priority = 20,{
    if(isFileUpload()){
      validFilename <- tablesFile()[["valid"]]
      validFiles <- input$files[input$files$name %in% validFilename,]
      processFiles(validFilename,validFiles,tableName(),input$domtype,pool)
    }
    else
    {
      out <- hot_to_r(input$table)
      uploadData(out,isolate(tableName()),pool)  
    }
    shinyjs::reset("entry_form")
    removeModal()
  })
  
  observeEvent(input$table$changes$changes,{
    for (cell in input$table$changes$changes) {
      if(!identical(cell[[3]],cell[[4]]))
      {
        id <- cell[[1]]+1
        values <- c(updatedRows(),id)
        updatedRows(values)
      }
    }
  })
  
  # observeEvent(input$toggleSidebar,{
  #   toggle("sidebar")
  # })
}

# Change the column names of the dataframe to database friendly names
# Save the dataframe into the database
# Parameters : 
# - df : the dataframe to save
# - tablename : the name of the table
# - pool : the database connection pool
uploadData <- function(df,tablename,pool){
  out <- setDefaultColumnName(df,tablename)
  status <- saveData(out,tablename,pool)
  if (status)
    saveLog("upload","Nino",paste0("Upload data ",tablename," in the database"))
  else
    saveLog("upload","Nino",paste0("FAILED Upload data ",tablename," in the database"))
}

# Return the dataframe updated ids as list
# Parameters : 
# - data : the dataframe to retrieve the updated ids from
# Return the list of updated ids
getUpdatedIDs <- function(data){
  l <- data[[updatedRows(),1]]
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