


managementSidebarUI <- function(id) {
  # Create the UI for the downloadTab module
  # Parameters:
  #  - id: String, the module id
  ns <- NS(id)
  div(
    selectInput(ns("type"),label = "Select a data type",choices = c("Glacier" = "gl","Location"="loc","Patch"="patch")),
    hidden(textInput(ns("glacier"),"Glacier")),
    hidden(textInput(ns("location"),"Location")),
    hidden(textInput(ns("patch"),"Patch")),
    hidden(actionButton(ns("search"),"Search"))
  )

}

managementSidebar <- function(input, output, session){
  
  observeEvent(input$type,{
    type <- input$type
    if(type == "gl")
    {
      showElement("glacier")
      hideElement("location")
      hideElement("patch")
    }else{
      if(type == "loc")
      {
        showElement("glacier")
        showElement("location")
        hideElement("patch")
      }
      else
      {
        if(type == "patch"){
          showElement("glacier")
          showElement("location")
          showElement("patch")
        }
      }
    }
  })
}