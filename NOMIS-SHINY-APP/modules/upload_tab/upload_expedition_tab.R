
uploadExpeditionTabUI <- function(id){
  ns <- NS(id)
  
  panel(
    h1("Create a new expedition"),
    textInput(ns("name"),"Expedition's name : "),
    textInput(ns("abr"),"Abreviation : "),
    numericRangeInput(ns("range"),"Select a glacier range :",value = c(1,500)),
    actionButton(ns("add"),"Add range"),
    selectizeInput(
      inputId =  ns('ranges'),
      choices=c(''),
      label = 'Ranges',
      multiple = TRUE,
      options = list(
        'plugins' = list('remove_button')
      ),
    )
  )
}


uploadExpeditionTab <- function(input,output,session,pool,dimension){
  
  rangeVector <- ''
  
  observeEvent(input$add,{
    newRange <- isolate(input$range)
    print(rangeVector)
    rangeVector <- append(rangeVector,paste0(newRange[1]," - ",newRange[2]))
    print(rangeVector)
    updateSelectizeInput(session,"ranges",choices = rangeVector,selected = rangeVector)
  })
}