
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
manageDataTabUI <- function(id,title,pool){
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
                           "List of glaciers" = "list",
                           "Expedition"="expedition")),
                textInput(ns("glacier"),"Enter glacier ID"),
                hidden(numericRangeInput(ns("glacierRange"),label = "Range of glaciers", value = c(MIN,MAX))),
                hidden(textInput(ns("glacierList"),"List of glaciers (comma separated)")),
                hidden(selectInput(ns("expedSelection"),label = "Select an expedition",choices = formatExpedList(pool)))),
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
            hidden(actionButton(ns("upload"),"Upload data",class="upload-button")),
            hidden(downloadButton(ns("download"),"Download template",class="upload-button")) 
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
  existingExpedFile <- reactiveVal()
  updatedRows <- reactiveVal(vector())
  isFileUpload <- reactiveVal(FALSE)
  isFileExpedUpload <- reactiveVal(FALSE)
  update <- reactiveVal(FALSE)
  expeditions <- reactiveVal()
  dfToDownload <- reactiveVal()
  # Reactive variable
  tableName <- reactive(getTableNameFromValue(input$type))
  selectedFields <- reactive(getFieldsFromValue(input$type))
  selectType <- reactive({input$selectRange})
  filenames <- reactive({generateFilenames(ids(),input$domtype)})
  existingFiles <- reactive({getExistingFilenamesInDB(pool,tableName(),input$domtype,ids())})
  tablesFile <- reactive({generateFileTables(filenames(),input$files,existingFiles(),isUploadOnly)})
  # Reactive variable which contains the dataframe to display
  dataf <- reactive({
    if(input$type != "expedition" && input$type != "biogeo_3u"){
      update()
      if(input$type %in% tableList)
        tmp <- getTableFromGlacier(pool,tableName(),ids())
      else
        tmp <- getFieldsFromGlacier(pool,tableName(),selectedFields(),ids())
      # print(tmp)
      return(generateFilledDF(tmp,tableName(),ids()))
    }

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
                # print(ranges[[i,"min"]])
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
  },ignoreInit = TRUE)
  
  
  w <- Waiter$new(id=session$ns("table-container"),html = spin_throbber(),color = "#FFFFFF80")
  
  # observeEvent that react to generate input's update
  # Generate and render the data table 
  observeEvent(input$generate,{
    output$table <- renderRHandsontable({
      if(input$type != "expedition" && input$type != "biogeo_3u"){
        validateInput(input)
        w$show()
        table <- isolate(input$type)
        df <- dataf()
        # print(df)
        dfToDownload(formatTemplateForDownload(df,table))
        if(isUploadOnly)
          readOnlyCells <- getReadOnlyCells(df,tableName())
        else
          readOnlyCells <- vector()
        table <-generateHandsonTable(df,dimension,readOnlyCells,table,isolate(tableName()))
        w$hide()
        table
      }
      
    })
    showElement("upload")
    showElement("download")
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
      hideElement("download")
    }
    else if(input$type == "expedition" || input$type == "trace_el" || input$type == "ft_icr_ms" ){
      if(input$type == "expedition"){
        updateSelectInput(session,"domtype",label = "Select a DNA parameter",choices = uploadDNAFilesType)
        showElement("domtype")
      }
      else if(input$type == "trace_el" || input$type == "ft_icr_ms"){
        hideElement("domtype")
      }
      expeditions(formatExpedList(pool))
      updateSelectInput(session,"expedition",label = "Select an expedition",choices = expeditions())
      output$fileContainer <- renderUI({fileInput(session$ns("files"),"Select file",accept=".txt,.tre,.nwk,.tree,.treefile",multiple = FALSE)}) 
      w$hide()
      isFileUpload(TRUE)
      isFileExpedUpload(TRUE)
      hideElement("glacierSelection")
      showElement("expedition")
      hideElement("spinner")
      showElement("fileContainer")
      hideElement("generate")
      hideElement("table")
      hideElement("upload")
      hideElement("download")
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
    output$fileValid <- renderTable({ 
      data.frame("Valid" =c())})
    output$fileWrong <- renderTable({ 
      data.frame("Wrong" =c())})
    output$fileMissing <- renderTable({ 
      data.frame("Missing" =c())})
    output$fileExisting <- renderTable({ 
      data.frame("Existing" =c())})
    if(isFileExpedUpload()){
      if(isExtensionValid(input$files$name,input$domtype)) {
        output$fileValid <- renderTable({ 
          data.frame("Valid" =input$files$name)})
        enable("uploadFiles")
      }
      else{
        output$fileValid <- renderTable({ 
          data.frame("Wrong" =input$files$name)})
        disable("uploadFiles")
      }
      if(input$type == "trace_el" || input$type == "ft_icr_ms")
        fileType <- input$type
      else
        fileType <- input$domtype
      exp <-getTable("expedition",pool)
      existingExpedFile(exp[exp$id_expedition == as.numeric(input$expedition),][[fileType]])
      if(!is.na(existingExpedFile())){
        output$fileExisting <- renderTable({ 
          data.frame("Existing" =existingExpedFile())})
        if(isUploadOnly)
          disable("uploadFiles")
      }
      else{
        output$fileExisting <- renderTable({ 
          data.frame("Existing" =c())})
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
    if(!is.null(input$table)){
      out <- hot_to_r(input$table)
      if(!isUploadOnly)
        show_validation_popup(tableName(),updatedRows(),output,session$ns,FALSE,out)
      else{
        w$show()
        result <- uploadData(out,isolate(tableName()),isolate(input$type),pool)
        if(result)
          update(!update())
        else
          w$hide()
      }
    }
    
  },ignoreInit = TRUE)
  
  # Handler that allow user to download the previously generated file
  output$download <- downloadHandler(
    filename = function() {
      paste("nomis-",format(Sys.time(), "%Y%m%d-%H%M"),'-template', ".csv", sep = "")
    },
    content = function(file) {
      write.csv(isolate(dfToDownload()), file, row.names = FALSE,na = "NA")
    }
  )
  
  # observeEvent that react to uplooadFiles button click
  # save valid files on the server
  observeEvent(input$uploadFiles,{
    if(isFileExpedUpload()){
      validFilename <- input$files$name
      validFilePath <- input$files$datapath
      if(input$type == "trace_el" || input$type == "ft_icr_ms"){
        fileType <- input$type
        table <- "expedition"
      }else{
        fileType <- input$domtype
        table <- tableName()
      }
      if(!isUploadOnly)
        show_validation_popup(table,validFilename,output,session$ns,TRUE)
      else{
        saveFile(validFilename,validFilePath,"data/",type = fileType)
        expedName <- names(expeditions()[expeditions()==input$expedition])
        saveField(table,fileType,input$expedition,expedName,validFilename,pool)
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
        if(input$type == "trace_el" || input$type == "ft_icr_ms"){
          fileType <- input$type
          table <- "expedition"
        }else{
          fileType <- input$domtype
          table <- tableName()
        }
        validFilename <- input$files$name
        validFilePath <- input$files$datapath
        saveFile(validFilename,validFilePath,"data/",type = fileType)
        expedName <- names(expeditions()[expeditions()==input$expedition])
        saveField(table,fileType,input$expedition,expedName,validFilename,pool)
        if(!is.na(existingExpedFile()))
          deleteFile(paste0("data/",fileType,"/"),existingExpedFile())
      }
      else{
        validFilename <- tablesFile()[["valid"]]
        validFiles <- input$files[input$files$name %in% validFilename,]
        processDOMFiles(validFilename,validFiles,tableName(),input$domtype,pool)
      }
    }
    else
    {
      if(!is.null(input$table)){
        out <- hot_to_r(input$table)
        w$show()
        result <- uploadData(out,isolate(tableName()),isolate(input$type),pool)
        if(result)
          update(!update())
        else
          w$hide()
      }
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
# Return true if success
uploadData <- function(df,tablename,listName,pool){
  # print(df)
  out <- setDefaultColumnName(df,listName)
  out <- formatDFforUpload(out)
  isValid <- findInvalidCells(out,listName)
  if(isValid){
    status <- saveData(out,tablename,TRUE,pool)
    if (status)
      saveLog("upload","Nino",paste0("Upload data ",tablename," in the database"))
    else
      saveLog("upload","Nino",paste0("FAILED Upload data ",tablename," in the database"))
    
    return(TRUE)
  }
  else
    return(FALSE)
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

