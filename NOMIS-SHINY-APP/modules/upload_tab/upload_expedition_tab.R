
uploadExpeditionTabUI <- function(id){
  ns <- NS(id)
  fluidRow(
    column(width = 6,offset = 3,
          panel(class="panel",
            h1(class="center","Create a new expedition"),
            div(class="exped",textInput(ns("name"),"Expedition's name : ")),
            div(class="exped",textInput(ns("abr"),"Abreviation : ")),
            div(
              div(class="inline exped",numericRangeInput(ns("range"),"Select a glacier range :",value = c(1,500))),
              div(class="btn",actionButton(ns("add"),"Add range"))
            ),
            div(class="exped",selectizeInput(
              inputId =  ns('ranges'),
              choices=c(''),
              label = 'Ranges',
              multiple = TRUE,
              options = list(
                'plugins' = list('remove_button')
              ),
            )
          ),
          div(class="center",actionButton(ns("create"),"Create expedition"))
        )
    )
  )

}


uploadExpeditionTab <- function(input,output,session,pool,dimension){
  
  rangeVector <- reactiveVal(vector())
  
  observeEvent(input$add,{
    newRange <- isolate(input$range)
    tmp <- c(rangeVector(),paste0(newRange[1]," - ",newRange[2]))
    rangeVector(tmp)
    updateSelectizeInput(session,"ranges",choices = rangeVector(),selected = rangeVector())
  })
  
  observeEvent(input$create,{
    
    saveExpeditionInDB(input$name,input$abr,pool)
    
    for (range in input$ranges) {
      range <- str_split(range," - ")
      saveRangeInDB(input$name,as.numeric(range[[1]][1]),as.numeric(range[[1]][2]),pool)
    }
  })
}