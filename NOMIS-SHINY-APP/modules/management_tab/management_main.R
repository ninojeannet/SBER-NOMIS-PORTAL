

managementMainUI <- function(id) {
  # Create the UI for the downloadTab module
  # Parameters:
  #  - id: String, the module id
  ns <- NS(id)
  selectInput(ns("typeOfData"),label = "Select a data type",choices = c("Glacier" = "gl","Location"="loc","Patch"="patch"))
  
  # renderText(ns("tmp"),{"test"})
  
}

managementMain <- function(input, output, session){
  
}