


managementSidebarUI <- function(id) {
  # Create the UI for the downloadTab module
  # Parameters:
  #  - id: String, the module id
  ns <- NS(id)
  div(
    selectInput(ns("type"),label = "Select a data type",choices = c("Glacier" = "glacier","Location"="location","Patch"="patch","Enzyme"="enzyme")),
    hidden(textInput(ns("glacier"),"Glacier",value = "")),
    hidden(textInput(ns("location"),"Location")),
    hidden(textInput(ns("patch"),"Patch")),
    actionButton(ns("searchh"),"Search"),
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
  
  return(list(
    id = reactive({input$glacier}),
    btnClick = input$searchh
  ))
}