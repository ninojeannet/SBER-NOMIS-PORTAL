
managementProgressTabUI <- function(id) {
  # Parameters:
  #  - id: String, the module id
  ns <- NS(id)
  
  # textInput(ns("test"),"test")
  formattableOutput(ns("progress_table"))
}

managementProgressTab <- function(input, output, session,pool){
  rangeList <- reactiveVal(list())
  
  df <- reactive({
    tmp <- isolate(rangeList())
    
    dataframe <- getProgressTable(pool)
    for (i in 1:nrow(dataframe)) {
      row <- dataframe[i,]
      abr <- row[["abreviation"]]
      tmp[[abr]] <- append(tmp[[abr]],list(c(row[["min"]],row[["max"]]))) 
    }
    rangeList(tmp)
    print(isolate(rangeList()))
    
    dataframe$range<-paste(dataframe$min, dataframe$max, sep=" - ")
    as.data.frame(t(dataframe))
    
  })
  #TODO peut être ne pas faire ça ici. -> enregistrer directement en string 12/12 jsp a voir
  
  output$progress_table <- renderFormattable({
    formattable(df())
  })
}


