## This module contains the UI and server code for the Download tab

## Source needed files ############################################################
#source('./modules/download_tab/download_data.R')
#source('./modules/download_tab/request_data.R')
source('./utils/template_config.R')

## Create module UI ###############################################################

uploadTabUI <- function(id) {
  # Create the UI for the downloadTab module
  # Parameters:
  #  - id: String, the module id
  ns <- NS(id)
  
  types <- c("Glacier metrics"="gl","Location metrics"="loc","Patch metrics"="patch")
  
  wellPanel(
    selectInput(ns('templateType'),"Select a file template",choices = types),
    fileInput(ns("file"),"Select your file (.csv)",accept=".csv"),
    disabled(actionButton(ns("btnUpload"),"Insert data in database",icon = icon("upload"))),
    textOutput(ns("status")),
    tableOutput(ns("table"))
    
  )
  
  # Create namespace
  
}

uploadTab <- function(input, output, session,pool){

  #

  observeEvent(input$file,{
    file <- input$file
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    if(ext != "csv"){
      statusText <- "Please upload a csv file"
      disable("btnUpload")
    } 
    else
    {
      statusText <- ""
      enable("btnUpload")
    }
    output$status <- renderText({
      statusText
    })
  })
  
  observeEvent(input$btnUpload, {
    file <- input$file
    content <- read.csv(file$datapath)
    type <- input$templateType

    output$status <- renderText({
      c(" test ",type)
    })
    if (isFileValid(output,content,type))
    {
      statusText <- "File format is valid"
      saveData(content,"glacier",output,pool,session)
    }
    else
      statusText <- "File format is not valid for the selected template"
    
    
    output$status <- renderText({
      statusText
    })

  })
}


isFileValid <- function(output,content, type){
  
  if(match(type,names(templateConfig)))
  {
    if(identical(colnames(content),templateConfig[[type]]))
    {
      return(TRUE)
    }
  }
  return(FALSE)
}

saveData <- function(data,tableName,output,pool,session){
  # output$table <- renderTable({
  #   dbGetQuery(pool,"SELECT * FROM glacier")
  # })
  
  #dbAppendTable(pool,"glacier",data)
  responses_df <- data.frame(row_id = character(),
                             name = character(),
                             sex = character(),
                             age = character(), 
                             comment = character(),
                             date = as.Date(character()),
                             stringsAsFactors = FALSE)
  # dbWriteTable(pool, "responses_df", responses_df, overwrite = FALSE, append = TRUE)

  
  request <- paste0(c('INSERT INTO ',tableName,' ('))
  headers <-colnames(data)
  for(x in headers){request <- paste0(c(request,"`",x,"`, "))}
  
  request <- paste(request,collapse = '')

  # for (variable in colnames(data)) {
  #   request <- paste0(request,sprintf("'",variable,"',"))
  # }
  request <- substr(request,1,nchar(request)-2)
  request <- paste0(request,") VALUES ")
  # 
  # 
  for(row in 1:nrow(data)){
    request <- paste0(request,"(")
    for(value in data[row,])
    {
        request <- paste0(request,"'",value,"',")
    }
    request <- substr(request,1,nchar(request)-1)
    request <- paste0(request,"),")
  }
  request <- substr(request,1,nchar(request)-1)
  request <- paste0(request,";")
  session$sendCustomMessage(type = 'testmessage',{request} )
  
  conn <- poolCheckout(pool)
  dbWithTransaction(conn,{
    dbGetQuery(conn,request)
  })
  
  poolReturn(conn)
}


