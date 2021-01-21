
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
manageDataTabUI <- function(id,title){
  ns <- NS(id)
  
  div(
    div(
      class= 'main-inputs',
      h1(title),
      div(
        class = 'main-actions',
        actionButton(ns('help'), 'Help', class = 'help custom-style custom-style--primary')
      )),
      # Create the sidebarLayout
      sidebarLayout(
        sidebarPanel(
          id = ns('sidebar'),
          div(
            selectInput(ns("type"),label = "Select a data type",choices = uploadDataTypes),
            hidden(selectInput(ns("domtype"),label = "Select a DOM parameter",choices = uploadDOMTypes)),
            hidden(selectInput(ns("expedition"),label = "Select an expedition",choices = c())),
            div(id=ns("glacierSelection"),
                radioButtons(ns("selectRange"), "Choose a selection option :",
                         c("Unique glacier" = "simple",
                           "Range of glaciers" = "range",
                           "List of glaciers" = "list")),
                textInput(ns("glacier"),"Enter glacier ID"),
                hidden(numericRangeInput(ns("glacierRange"),label = "Range of glaciers", value = c(MIN,MAX))),
                hidden(textInput(ns("glacierList"),"List of glaciers (comma separated)"))),
            actionButton(ns("generate"),"Generate template"),
            hidden(uiOutput(ns("fileContainer")))
          ),
          width = 3
        ),
        # Create the main panel
        mainPanel(
          id = ns('main'),
          div(id="main-content",
            hidden(h1(id = ns("title"),"Selected files status")),
            div(class = "file-output",id=ns("tables"),
                div(class="file-table",tableOutput(ns("fileValid"))),
                div(class="file-table",tableOutput(ns("fileWrong"))),
                div(class="file-table",tableOutput(ns("fileMissing"))),
                div(class="file-table",tableOutput(ns("fileExisting"))),
                div(class="file-table",hidden(actionButton(ns("uploadFiles"),"Upload valid files",class="upload-button")))
            ),
            use_waiter(),
            div(id =ns("table-container"),class="table-container",rHandsontableOutput(ns("table"))),
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
# isUploadOnly : boolean indicating if this module is for upload only or not
manageDataTab <- function(input,output,session,pool,dimension,isUploadOnly){
  # Reactive value
  sidebarVisible <- reactiveVal(TRUE)
  updatedRows <- reactiveVal(vector())
  isFileUpload <- reactiveVal(FALSE)
  isFileExpedUpload <- reactiveVal(FALSE)
  update <- reactiveVal(FALSE)
  expeditions <- reactiveVal()
  # Reactive variable
  tableName <- reactive(getTableNameFromValue({input$type}))
  selectedFields <- reactive(getFieldsFromValue(input$type))
  selectType <- reactive({input$selectRange})
  filenames <- reactive({generateFilenames(ids(),input$domtype)})
  existingFiles <- reactive({getExistingFilenamesInDB(pool,tableName(),input$domtype,ids())})
  tablesFile <- reactive({generateFileTables(filenames(),input$files,existingFiles(),isUploadOnly)})
  # Reactive variable which contains the dataframe to display
  dataf <- reactive({
    update()
    if(input$type %in% tableList)
      tmp <- getTableFromGlacier(pool,tableName(),ids())
    else
      tmp <- getFieldsFromGlacier(pool,tableName(),selectedFields(),ids())
    return(generateFilledDF(tmp,tableName(),ids()))
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
  },ignoreInit = TRUE)
  
  
  w <- Waiter$new(id=session$ns("table-container"),html = spin_throbber(),color = "#FFFFFF80")
  
  # observeEvent that react to generate input's update
  # Generate and render the data table 
  observeEvent(input$generate,{
    
    output$table <- renderRHandsontable({
      validateInput(input)
      w$show()
      table <- isolate(input$type)
      df <- dataf()
      if(isUploadOnly)
        readOnlyCells <- getReadOnlyCells(df,tableName())
      else
        readOnlyCells <- vector()
      table <-generateHandsonTable(df,dimension,readOnlyCells,table,isolate(tableName()))
      w$hide()
      table
    })
    showElement("upload")
  },ignoreInit = TRUE)
  
  # observeEvent that react to type input's update
  # Hide / show html elements
  observeEvent(input$type,{
    if (input$type == "biogeo_3u"){
      updateSelectInput(session,"domtype",label = "Select a DOM parameter",choices = uploadDOMTypes)
      output$fileContainer <- renderUI({fileInput(session$ns("files"),"Select files",accept=".dat",multiple = TRUE)}) 
      w$hide()
      isFileUpload(TRUE)
      isFileExpedUpload(FALSE)
      showElement("glacierSelection")
      hideElement("expedition")
      hideElement("spinner")
      showElement("domtype")
      showElement("fileContainer")
      hideElement("generate")
      hideElement("table")
      hideElement("upload")
    }
    else if(input$type == "expedition"){
      updateSelectInput(session,"domtype",label = "Select a DNA parameter",choices = c("16s table"="16s"))
      exped <-getTable("expedition",pool)
      expeditions(setNames(as.character(exped$id_expedition), exped$name))
      updateSelectInput(session,"expedition",label = "Select an expedition",choices = expeditions())
      output$fileContainer <- renderUI({fileInput(session$ns("files"),"Select file",accept=".txt",multiple = FALSE)}) 
      w$hide()
      isFileUpload(TRUE)
      isFileExpedUpload(TRUE)
      hideElement("glacierSelection")
      showElement("expedition")
      hideElement("spinner")
      showElement("domtype")
      showElement("fileContainer")
      hideElement("generate")
      hideElement("table")
      hideElement("upload")
    }
    else{
      isFileUpload(FALSE)
      showElement("glacierSelection")
      hideElement("expedition")
      hideElement("domtype")
      hideElement("fileContainer")
      hideElement("title")
      hideElement("uploadFiles")
      hideElement("tables")
      showElement("generate")
      showElement("table")
    }
  },ignoreInit = TRUE)
  
  # observeEvent that react to files input's update
  # Generate and render lists of files (valid, missing, wrong, existing)
  observeEvent(input$files,{
    w$show()
    showElement("uploadFiles")
    showElement("title")
    showElement("tables")
    if(isFileExpedUpload()){
      if(!is.na(str_match(input$files$name,"\\.txt")[1])){
        output$fileValid <- renderTable({ 
          data.frame("Valid" =input$files$name)})
        enable("uploadFiles")
      }
      else{
        output$fileValid <- renderTable({ 
          data.frame("Wrong" =input$files$name)})
        disable("uploadFiles")
      }
    }
    else
    {
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
    }
    
    w$hide()
  },ignoreInit = TRUE)
  
  # observeEvent that react to upload button click
  # Save the data table in the database
  observeEvent(input$upload,{
    out <- hot_to_r(input$table)
    if(!isUploadOnly)
      show_validation_popup(tableName(),updatedRows(),output,session$ns,FALSE,out)
    else{
      w$show()
      uploadData(out,isolate(tableName()),isolate(input$type),pool)
      update(!update())
    }
  },ignoreInit = TRUE)
  
  # observeEvent that react to uplooadFiles button click
  # save valid files on the server
  observeEvent(input$uploadFiles,{
    if(isFileExpedUpload()){
      validFilename <- input$files$name
      validFilePath <- input$files$datapath
      if(!isUploadOnly)
        show_validation_popup(tableName(),validFilename,output,session$ns,TRUE)
      else{
        saveFile(validFilename,validFilePath,"data/",type = input$domtype)
        expedName <- names(expeditions()[expeditions()==input$expedition])
        print(expedName)
        saveField(input$type,input$domtype,input$expedition,expedName,validFilename,pool)
      }
    }
    else{
      validFilename <- tablesFile()[["valid"]]
      validFiles <- input$files[input$files$name %in% validFilename,]
      if(!isUploadOnly)
        show_validation_popup(tableName(),validFilename,output,session$ns,TRUE)
      else
        processDOMFiles(validFilename,validFiles,tableName(),input$domtype,pool)
    }
  },ignoreInit = TRUE)
  
  # observeEvent that react to submit button click
  # submit button is located in the validation popup window
  # Save updated data once validated
  observeEvent(input$submit, priority = 20,{
    shinyjs::reset("entry_form")
    removeModal()
    if(isFileUpload()){
      if(isFileExpedUpload()){
        validFilename <- input$files$name
        validFilePath <- input$files$datapath
        saveFile(validFilename,validFilePath,"data/",type = input$domtype)
        saveField(input$type,input$domtype,input$expediiton,validFilename,pool)
      }
      else{
        validFilename <- tablesFile()[["valid"]]
        validFiles <- input$files[input$files$name %in% validFilename,]
        processDOMFiles(validFilename,validFiles,tableName(),input$domtype,pool)
      }
    }
    else
    {
      out <- hot_to_r(input$table)
      w$show()
      uploadData(out,isolate(tableName()),isolate(input$type),pool)
      update(!update())
    }
  },ignoreInit = TRUE)
  
  # observeEvent that react to each update on the data table
  # Keep tracks of each updated rows
  observeEvent(input$table$changes$changes,{
    for (cell in input$table$changes$changes) {
      if(!identical(cell[[3]],cell[[4]]))
      {
        id <- cell[[1]]+1
        values <- c(updatedRows(),id)
        updatedRows(values)
      }
    }
  },ignoreInit = TRUE)
  
  # Create an observeEvent that react to the help button
  observeEvent(input$help, {
    # Create modal with the corresponding htmlOutput
    if(isUploadOnly){
      title <- 'Upload help'
      template <-'./html_components/help_upload_data.html'
    }
    else{
      title <- 'Data Management help'
      template <-'./html_components/help_manage_data.html'
    }
      
    showModal(modalDialog(
      title = title,
      htmlTemplate(template),
      footer = modalButtonWithClass('Dismiss', class = 'custom-style'),
      easyClose = TRUE
    ))
  },ignoreInit = TRUE)
}

# Change the column names of the dataframe to database friendly names
# Save the dataframe into the database
# Parameters : 
# - df : the dataframe to save
# - tablename : the name of the table
# - listName : the name of the list to display in the table
# - pool : the database connection pool
uploadData <- function(df,tablename,listName,pool){

  out <- setDefaultColumnName(df,listName)
  out <- formatDFforUpload(out)
  status <- saveData(out,tablename,TRUE,pool)

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
            "simple" = {need(grep('(\\d){1,3}',isolate(input$glacier)), 'Please insert a valid ID ( ex : 23 )')},
            "range" = {
              range <- isolate(input$glacierRange)
              need(range[2] > range[1] & range[1] > MIN-1 & range[2] < MAX+1, paste0('Please insert a valid range of id (between ',MIN,' and ',MAX,')'))},
            "list" = {need(grep('^[0-9]+(,[0-9]+)*$',isolate(input$glacierList)), 'Please insert a valid list of ID ( ex : 23,45,56 )')}
    )
  )
}

validateInputEmpty <- function(input){
  shiny::validate(
    switch (isolate(input$selectRange),
            "simple" = {need(grep('(\\d){1,3}',isolate(input$glacier)), '')},
            "range" = {
              range <- isolate(input$glacierRange)
              need(range[2] > range[1] & range[1] > MIN-1 & range[2] < MAX+1, '')},
            "list" = {need(grep('^[0-9]+(,[0-9]+)*$',isolate(input$glacierList)), '')}
    )
  )
}