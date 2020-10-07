## This module contains the UI and server code for the Download tab

source('./utils/template_config.R')
source('./utils/helper_database.R')


## Create module UI ###############################################################

uploadFileTabUI <- function(id) {
  # Create the UI for the downloadTab module
  # Parameters:
  #  - id: String, the module id
  ns <- NS(id)
  
  
  wellPanel(
    # selectInput(ns('templateType'),"Select a file template",choices = templateTypes),
    # fileInput(ns("file"),"Select your file (.csv)",accept=".csv"),
    # disabled(actionButton(ns("btnUpload"),"Insert data in database",icon = icon("upload"))),
    # textOutput(ns("status")),
    # tableOutput(ns("table"))
    
  )
}

uploadFileTab <- function(input, output, session,pool){

#   observeEvent(input$file,{
#     file <- input$file
#     ext <- tools::file_ext(file$datapath)
#     
#     req(file)
#     if(ext != "csv"){
#       statusText <- "Please upload a csv file"
#       disable("btnUpload")
#     } 
#     else
#     {
#       statusText <- ""
#       enable("btnUpload")
#     }
#     output$status <- renderText({
#       statusText
#     })
#   })
#   
# observeEvent(input$btnUpload, {
#     file <- input$file
#     content <- read.csv(file$datapath)
#     # print(content)
#     type <- input$templateType
#     # content <- createIdColumn(content,type)
# 
#     if (isFileValid(output,content,type))
#     {
#       statusText <- ""
#       saveData(content,type,output,pool,session)
#     }
#     else
#       statusText <- "File format is not valid for the selected template"
#     
#     output$status <- renderText({
#       statusText
#     })
# 
#   })
}



# Check if the content of the uploaded file fit the selected template.
isFileValid <- function(output,content, type){
  # tmp <- sapply(colnames(content), removeBrackets)
  # showNotification(str_replace(colnames(content)[2],"\\[.*\\]",""))
  
  
  if(match(type,names(templateFieldNames)))
  {
    if(identical(colnames(content),templateFieldNames[[type]]))
    {
      return(TRUE)
    }
  }
  return(FALSE)
}



# Save the given data into the database
# Params :
# - data : dataframe of data to insert into the database
# - tableName : name of the table in which the data will be inserted
# - pool : the pool of connection to communicate with the database
# saveData <- function(data,tableName,output,pool,session){
# 
#   request <- buildInsertQuery(data,tableName)
# 
#   #Send query to the database using pool
#   check <- tryCatch({
#     conn <- poolCheckout(pool)
#     queryStatus <- dbWithTransaction(conn,{
#       dbGetQuery(conn,request)
#     })
#     poolReturn(conn)
#     print("Data successfully inserted into database")
#     showNotification("Data successfully inserted into database",type = "message")
#   },
#   warning = function(war){
#     print(war)
#     showNotification(war$message, type = "warning")
#   },
#   error = function(err){
#     print(err)
#     showNotification(err$message,type = "error",duration = NULL)
#   },
#   finally = function(f){
#     print(e) 
#   })
#   
# }


# Build an insert sql query
# Params :
# - data : dataframe of data to insert into the database
# - tableName : name of the table in which the data will be inserted
# Return the built request in a string
# buildInsertQuery <- function(data,tableName){
#   request <- paste0(c('INSERT INTO ',tableName,' ('))
#   headers <-colnames(data)
#   for(x in headers){request <- paste0(c(request,"`",x,"`, "))}
#   request <- paste(request,collapse = '')
#   request <- substr(request,1,nchar(request)-2)
#   request <- paste0(request,") VALUES ")
#   for(row in 1:nrow(data)){
#     request <- paste0(request,"(")
#     for(value in data[row,])
#     {
#       if(is.na(value))
#         request <- paste0(request,"NULL,")
#       else
#         request <- paste0(request,"'",value,"',")
#     }
#     request <- substr(request,1,nchar(request)-1)
#     request <- paste0(request,"),")
#   }
#   request <- substr(request,1,nchar(request)-1)
#   request <- paste0(request," AS new_values ON DUPLICATE KEY UPDATE ")
#   for(x in headers[-1]){request <- paste0(request,x,"=new_values.",x,", ")}
#   request <- substr(request,1,nchar(request)-2)
#   print(request)
#   return(request)
# }
# 
# removeBrackets <- function(value){
#   str_replace(value,"\\[.*\\]","")
#   print(value)
#   return(value)
# }


createIdColumn <- function(content,type){
  field1 <-uniqueTogetherFields[[type]][1]
  field2 <-uniqueTogetherFields[[type]][2]
  new_col = paste0(content[[field1]],"_",content[[field2]])
  new_col_name = paste("id_",type)
  content[[new_col_name]] = new_col
  content[,templateFieldNames[[type]]]
  return(content)
}

