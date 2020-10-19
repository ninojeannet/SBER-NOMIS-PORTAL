
uploadExpeditionTabUI <- function(id){
  ns <- NS(id)
  fluidRow(
    column(width = 6,offset = 3,class="expedition",
          panel(class="exped-content",
            h1(class="center","Create a new expedition"),
            div(class="exped",textInput(ns("name"),"Expedition's name : ")),
            div(class="exped",textInput(ns("abr"),"Abreviation : ")),
            div(
              div(class="inline exped",numericRangeInput(ns("range"),"Select a glacier range :",value = c(1,500))),
              div(class="btn-exped",actionButton(ns("add"),"Add range")),
              textOutput(ns("status"))
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
  
  iv <- InputValidator$new()
  iv$add_rule("name", sv_required())
  iv$add_rule("abr", sv_required())
  iv$add_rule("ranges", sv_required())

  iv_range <-InputValidator$new()
  iv_range$add_rule("range", ~ if (!is_valid_range(.)) "Please insert a valid range of glacier")
  iv_range$enable()
  is_valid_range <- function(newRange) {
    b <- newRange[2] > newRange[1] & newRange[2] > 0 & newRange[1] > 0 & !is.na(newRange[1]) & !is.na(newRange[2])
    if(!b)
      disable("add")
    else
      enable("add")
    return(b)
  }
  
  observeEvent(input$ranges,{
    rangeVector(input$ranges)
  },ignoreNULL  = FALSE)
  
  observeEvent(input$add,{
    newRange <- isolate(input$range)
    tmp <- c(rangeVector(),paste0(newRange[1]," - ",newRange[2]))
    rangeVector(tmp)
    updateSelectizeInput(session,"ranges",choices = rangeVector(),selected = rangeVector())
  })
  
  observeEvent(input$create,{
    if(iv$is_valid()){
      iv$disable()
      saveExpeditionInDB(input$name,input$abr,pool)
      for (range in input$ranges) {
        range <- str_split(range," - ")
        saveRangeInDB(input$name,as.numeric(range[[1]][1]),as.numeric(range[[1]][2]),pool)
      }
    }
    else{
      iv$enable()
    }
  })
}