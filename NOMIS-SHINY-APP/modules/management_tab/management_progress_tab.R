source('./utils/helper_refresh_progress.R')

managementProgressTabUI <- function(id) {
  # Parameters:
  #  - id: String, the module id
  ns <- NS(id)
  div(
    div(
      class= 'main-inputs',
      h2("test"),
      div(
        class = 'main-actions',
        actionButton(ns('refresh'), 'Refresh', class = 'help custom-style custom-style--primary')
      )),
    formattableOutput(ns("progress_table"))
  )
}

managementProgressTab <- function(input, output, session,pool){
  rangeList <- reactiveVal(list())
  
  df <- reactive({
    dataframe <- getProgressTable(pool)
  })
  
  dfFormatted <- reactiveVal(buildDF(pool))

  output$progress_table <- renderFormattable({
    dff <- dfFormatted()
    formattable(dff,align =c(rep("c",ncol(dff))), list(area(col = 1:ncol(dff)) ~ color_tile_valid()))
  })
  
  observeEvent(input$refresh,{
    dataframe <- df()
    rangeList(setRanges(dataframe))
    updateExpeditionTable(dataframe,rangeList(),pool)
    dfFormatted(buildDF(pool))
  })
  
  # Create an observeEvent that react to the help button
  observeEvent(input$help, {
    # Create modal with the corresponding htmlOutput
    title <- 'Project progress help'
    template <-'./html_components/help_manage_progression.html'
    
    showModal(modalDialog(
      title = title,
      htmlTemplate(template),
      footer = modalButtonWithClass('Dismiss', class = 'custom-style'),
      easyClose = TRUE
    ))
  })
}

buildDF <- function(pool){
  dataframe <- getProgressTable(pool)
  headers <- dataframe[["name"]]
  dataframe$range<-paste(dataframe$min, dataframe$max, sep=" - ")
  df <- dataframe %>% select(-min) %>% select(-max) %>% select(-name) %>% select(-id_expedition)
  df <- aggregate(df["range"], by=list(abreviation=df$abreviation,enzyme =df$enzyme,doc=df$doc,dom=df$dom), paste)
  df <- as.data.frame(t(df))
  colnames(df) <- sort(unique(headers))
  return(df)
}


color_tile_valid <- function (...) {
  formatter("span", style = function(x) {
    style( padding = "0 4px", 
          `border-radius` = "4px", 
          `background-color` = ifelse(param_validator(x), "lightgreen", "white"))
  })}


param_validator <- function(list){
  result <- vector()
  for (x in list) {
    if(grepl("/", x, fixed = TRUE)){
      values <- str_split(x," / ")
      if(values[[1]][1]==values[[1]][2])
        result <- c(result,TRUE)
      else
        result <- c(result,FALSE)
    }
    else
      result <- c(result,FALSE)
  }
  return(result)
}

setRanges <- function(dataframe){
  
  tmp <- list()
  
  for (i in 1:nrow(dataframe)) {
    row <- dataframe[i,]
    abr <- row[["abreviation"]]
    tmp[[abr]] <- append(tmp[[abr]],list(c(row[["min"]],row[["max"]]))) 
  }
  # print(ranges)
  return(tmp)
}
