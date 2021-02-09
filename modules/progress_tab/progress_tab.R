source('./utils/helper_expedition.R')

# UI side of the ProgressTab module
# Parameters:
#  - id: String, the module id
progressTabUI <- function(id) {

  ns <- NS(id)
  div(
    div(
      class= 'main-inputs',
      h1("Database progression"),
      div(
        class = 'main-actions',
        actionButton(ns('help'), 'Help', class = 'help custom-style custom-style--primary'),
        actionButton(ns('refresh'), 'Refresh', class = 'help custom-style custom-style--primary')
        
      )),
    div(class="progresstable",
    formattableOutput(ns("progress_table")))
  )
}

# Server side of the ProgressTab module
# Parameters : 
# input : input of the shiny app
# output : output of the shiny app
# session : session of the shiny app
# pool : connection pool to access the database
progressTab <- function(input, output, session,pool){
  # Reactive value
  rangeList <- reactiveVal(list())
  dfFormatted <- reactive({    
    refreshTable()
    buildProgressTable(pool)})
  
  refreshTable <- reactiveVal(FALSE)

  # Reactive variable
  getDF <- function(){
    df <- getProgressTable(pool)
    df <- df %>% select(-"16s") %>% select(-"18s") %>% select(-"trace_el") %>% select(-"ft_icr_ms")
    return(df)
  }
  
  # Render the data table of the overall project progress
  output$progress_table <- renderFormattable({
    dff <- dfFormatted()
    nbCol <- ncol(dff)
    # if(nrow(dff)==0){
    #   dff <- data.frame()
    #   nbCol<- 10}
    shiny::validate(need(nrow(dff)!= 0, 'No expedition available'))
    formattable(dff,align =c("l",rep("c",nbCol-1)), list(
      area(col = 1) ~ formatRowNames(),area(col = 2:nbCol) ~ color_tile_valid()))
  })
  
  # ObserveEvent that reacts to the refresh button click
  # Refresh the data table progress by fetching the database
  observeEvent(input$refresh,{
    dataframe <- getDF()
    if(nrow(dataframe) > 0){
      rangeList(setRanges(dataframe))
      updateExpeditionTable(dataframe,rangeList(),pool)
    }
    refreshTable(!refreshTable())
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

# Function that color cell according to cell content
color_tile_valid <- function (...) {
  formatter("span", style = function(x) {
    style( display = "inline-block",padding = "0 4px",
          `border-radius` = "4px", 
          `background-color` = ifelse(param_validator(x), "lightgreen", "white"),
          width = "80px")
  },x ~ icontext(ifelse(param_validator(x), "ok", ""),x))}

formatRowNames <- function(...){
  formatter("span", style = function(x) {
    style(display = "inline-block",width = "150px",`font-weight` = "bold") })
}

# Validator that checks if the cell should be colored or not
# Retunr boolean
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

# create and return a list of ranges from the given data frame
# PArameters : 
# - dataframe : the data frame from which create the list
# Return the list of ranges
setRanges <- function(dataframe){
  
  tmp <- list()
  for (i in 1:nrow(dataframe)) {
    row <- dataframe[i,]
    abr <- row[["abbreviation"]]
    tmp[[abr]] <- append(tmp[[abr]],list(c(row[["min"]],row[["max"]]))) 
  }
  return(tmp)
}
