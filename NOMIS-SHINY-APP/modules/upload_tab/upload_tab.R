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
      statusText <- "File format is valid"
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

saveFile <- function(){
  
}


