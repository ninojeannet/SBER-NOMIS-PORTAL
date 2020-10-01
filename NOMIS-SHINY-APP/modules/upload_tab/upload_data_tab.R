
source('./utils/helper_database.R')
source('./utils/helper_dataframe.R')
source('./utils/template_config.R')
source('./utils/dataframe_generator.R')

# UI function of the uploadDataTab module
# Parameters : 
# id : id of the module
uploadDataTabUI <- function(id){
  ns <- NS(id)
  
  # Create the sidebarLayout
  sidebarLayout(
    # Create a sidebar with the innerModule first unit input UI elements inside
    sidebarPanel(
      id = ns('sidebar-upload'),
      div(
        selectInput(ns("type"),label = "Select a data type",choices = c("Glacier" = "glacier","Location"="location","Patch"="patch","Enzyme"="enzyme")),
        radioButtons(ns("selectRange"), "Choose a selection option :",
                     c("Unique glacier" = "simple",
                     "Range of glacier" = "range",
                     "List of glacier" = "list")),
        textInput(ns("glacier"),"Enter glacier ID"),
        hidden(numericRangeInput(ns("glacierRange"),label = "Glacier range", value = c(1, 500))),
        hidden(textInput(ns("glacierList"),"Glacier list (comma separated)")),
        actionButton(ns("generate"),"Generate template"),
      ),
      width = 4
    ),
    # Create the main panel with the innerModule first unit plot UI elements inside
    mainPanel(
      id = ns('main-upload'),
      div(
        rHandsontableOutput(ns("table")),
        hidden(actionButton(ns("upload"),"Upload data",class="upload-button"))
      ),
      width = 8
    )
  )
}

# Server function of the uploadDataTab module
# Parameters : 
# input : input of the shiny app
# output : output of the shiny app
# session : session of the shiny app
# pool : connection pool to access the database
uploadDataTab <- function(input,output,session,pool,dimension){
  
  tableName <- reactive({input$type})
  selectType <- reactive({input$selectRange})

  dataf <- reactive({
    
    switch (input$selectRange,
      "simple" = {
        ids <- input$glacier
      },
      "range" = {
        range <- input$glacierRange
        ids <- c(paste0("GL",as.character(range[1]:range[2])))
      },
      "list" = {
        ids <- input$glacierList
        ids <- gsub(" ", "", ids, fixed = TRUE)
        ids <- strsplit(ids,',')
        ids <- sapply(ids, function(x){paste0("GL",x)})
      }
    )
    tmp <- getDataFromGlacier(pool,tableName(),ids)
    return(generateFilledDF(tmp,tableName(),ids))
  })
  
  observeEvent(input$selectRange,{
    switch (input$selectRange,
      "simple" = {
        showElement("glacier")
        hideElement("glacierRange")
        hideElement("glacierList")
      },
      "range" = {
        hideElement("glacier")
        showElement("glacierRange")
        hideElement("glacierList")
      },
      "list" = {
        hideElement("glacier")
        hideElement("glacierRange")
        showElement("glacierList")
      }
    )
    
  })
  
  observeEvent(input$generate,{
    output$table <- renderRHandsontable({
      validateInput(input)
      table <- isolate(tableName())
      df <- isolate(dataf())
      showElement("upload")
      
      # readOnlyRows <- getReadOnlyRows(df,tableName())
      # 
      rhandsontable(df,digits=10,overflow='visible',stretchH = "all", height = dimension()[2]/100*70)%>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)%>%
        hot_cols(format = tableOptions[[table]][["format"]],renderer = "
    function(instance, td, row, col, prop, value, cellProperties) {
      Handsontable.renderers.TextRenderer.apply(this, arguments);

      return td;
  }") %>%
        hot_col(readOnlyFields[[table]], readOnly = TRUE) 
        # %>%
        # hot_row(readOnlyRows, readOnly = TRUE)
      })
  })
  

  # observeEvent(input$table$changes$changes,{
  #   print(input$table$changes$changes)
  #   for (cell in input$table$changes$changes) {
  #     rows <- append(rows,cell[[2]])
  #   }
  # })
  # res <-observeEvent(input$hot_select,{
  #   print("test")
  # })
  
  observeEvent(input$upload,{
    out <- hot_to_r(input$table)
    saveData(out,isolate(tableName()),pool)
  })
}


# getReadOnlyRows <- function(dataframe,tablename){
#   rows <- c()
#   
#   colNames <- setdiff(unlist(templateFieldNames[tablename]),unlist(readOnlyFields[tablename]))
#   dataframe <- dataframe[colNames]
#   print(rows <- append(rows,i))
#   # for (i in 1:nrow(dataframe)) {
#   #   row <- dataframe[i,]
#   #   # print(dataframe[i,])
#   #   print(row)
#   #   # if(is.na(row)){
#   #   #   print(i)
#   #   #   rows <- append(rows,i)
#   #   # }
#   #   # if(!is.empty(row))
#   #   #   rows <- append(rows,i)
#   # }
#   return(rows)
# }

# Check if the input format are valid and if not display a message
# Parameters :
# input : input of the shiny app
validateInput <- function(input){
  shiny::validate(
    switch (isolate(input$selectRange),
            "simple" = {need(grep('GL(\\d){1,3}',isolate(input$glacier)), 'Please insert a valid ID ( ex : GL23 )')},
            "range" = {
              range <- isolate(input$glacierRange)
              need(range[2] > range[1] & range[2] > 0 & range[1] > 0, 'Please insert a valid range of id')},
            "list" = {need(grep('^[0-9]+(,[0-9]+)*$',isolate(input$glacierList)), 'Please insert a valid list of ID ( ex : 23,45,56 )')}
    )
  )
}

