
source('./utils/helper_database.R')
source('./utils/helper_dataframe.R')

source('./utils/template_config.R')



uploadDataTabUI <- function(id){
  ns <- NS(id)
  
  # Create the sidebarLayout
  sidebarLayout(
    # Create a sidebar with the innerModule first unit input UI elements inside
    sidebarPanel(
      id = ns('sidebar-upload'),
      div(
        selectInput(ns("type"),label = "Select a data type",choices = c("Glacier" = "glacier","Location"="location","Patch"="patch","Enzyme"="enzyme")),
        textInput(ns("glacier"),"Enter glacier ID"),
        # selectInput(ns("location"),"Select a location", choices = c("Up"="UP","Down"="DN")),
        # selectInput(ns("patch"),"Select a patch", choices = c(1,2,3)),
        actionButton(ns("generate"),"Generate template"),
      ),
      width = 4
    ),
    # Create the main panel with the innerModule first unit plot UI elements inside
    mainPanel(
      id = ns('main-upload'),
      div(
        rHandsontableOutput(ns("table")),
        hidden(actionButton(ns("upload"),"Upload data"))
      ),
      width = 8
    )
  )
  
}


uploadDataTab <- function(input,output,session,pool){
  
  tableName <- reactive({input$type})
  
  dataf <- reactive({
    tmp <- getDataFromGlacier(pool,isolate(tableName()),input$glacier)
    return(generateFilledDF(tmp,tableName(),input$glacier))
  })
  
  observeEvent(input$generate,{
    output$table <- renderRHandsontable({
      rhandsontable(dataf(),digits=10,stretchH = "all")%>%
        hot_cols(format = tableOptions[[tableName()]][["format"]]) %>%
        hot_col(readOnlyFields[[tableName()]], readOnly = TRUE) 
    })
    showElement("upload")
  })
  
  observeEvent(input$upload,{
    out <- hot_to_r(input$table)
    saveData(out,tableName(),pool)
  })

  
}

# This function generate a complete dataframe for a specific table and existing data
# parameters :
# - dataf : the existing data as a data frame
# - tablename : the name of data's table
# - glacierID : the id of the data's glacier
# Return the generated data frame
generateFilledDF <- function(dataf,tablename,glacierID){
  
  df <- switch (tablename,
    'glacier' = generateGlacierDF(dataf,glacierID),
    'location' = generateLocationDF(dataf,glacierID),
    'patch' = generatePatchDF(dataf,glacierID),
    generateParametersDF(dataf,tablename,glacierID)
  )
  str(df)
  return(df)
  
}

generateGlacierDF <- function(dataf,glacierID){
  primary <- tableOptions[["glacier"]][["primary"]]
  id <- glacierID
  
  nbCol <- ncol(dataf)
  nbRow <- nrow(dataf)
  
  # Create a new empty dataframe
  newdataf <- dataf
  if (nrow(dataf) != 0)
    newdataf[,]=matrix(ncol=ncol(newdataf), rep(NA, prod(dim(newdataf))))
  
  print(nrow(newdataf))
  # Add new rows to the df if necessary
  if(nbRow < 1)
  {
    print("add rows")
    newdataf <- addRows(newdataf,-1,1,nbCol)
  }
  print(newdataf)
  # Fill the df with generated columns
  newdataf[[primary]] <- id
  if(nrow(dataf) != 0)
    newdataf <- copyDFValuesTo(dataf,newdataf,primary)
  
  return(newdataf)
  

}

generateLocationDF <- function(dataf,glacierID){
  primary <- tableOptions[["location"]][["primary"]]
  fk_column <- tableOptions[["location"]][["FK"]]
  
  idUP <- paste0(glacierID,"_UP")
  idDN <- paste0(glacierID,"_DN")
  
  ids <- c(idDN,idUP)
  fk <- glacierID
  
  nbCol <- ncol(dataf)
  nbRow <- nrow(dataf)
  
  # Create a new empty dataframe
  newdataf <- dataf
  newdataf[,]=matrix(ncol=ncol(newdataf), rep(NA, prod(dim(newdataf))))
  
  # Add new rows to the df if necessary
  if(nbRow < length(ids))
  {
    newdataf <- addRows(newdataf,nbRow,length(ids))
  }
  
  # Fill the df with generated columns
  newdataf[[primary]] <- ids
  newdataf[[fk_column]] <- fk
  newdataf[["type"]] <- c("Down","Up")
  
  newdataf <- copyDFValuesTo(dataf,newdataf,primary)
  
  return(newdataf)
  
}

generatePatchDF <- function(dataf,glacierID){
  primary <- tableOptions[["patch"]][["primary"]]
  fk <- tableOptions[["patch"]][["FK"]]
  
  idUP <- paste0(glacierID,"_UP")
  idDN <- paste0(glacierID,"_DN")
  ids <- vector()
  idsFk <- vector()
  patches <- c(1,2,3)
  for (patch in patches) {
      idsFk <- c(idsFk,idUP)
      idsFk <- c(idsFk,idDN)
      ids <- c(ids,paste0(idUP,"_",patch))
      ids <- c(ids,paste0(idDN,"_",patch))
  }
  ids <- sort(ids)
  idsFk <- sort(idsFk)
  
  nbCol <- ncol(dataf)
  nbRow <- nrow(dataf)
  
  # Create a new empty dataframe
  newdataf <- dataf
  newdataf[,]=matrix(ncol=ncol(newdataf), rep(NA, prod(dim(newdataf))))
  
  # Add new rows to the df if necessary
  if(nbRow < length(ids))
  {
    newdataf <- addRows(newdataf,nbRow,length(ids))
  }
  
  # Fill the df with generated columns
  newdataf[[primary]] <- ids
  newdataf[[fk]] <- idsFk
  newdataf[["name"]] <- patches
  
  newdataf <- copyDFValuesTo(dataf,newdataf,primary)
  
  return(newdataf)
  
}

generateParametersDF <- function(dataf,tablename,glacierID){
  # Get the column names from config file
  primary <- tableOptions[[tablename]][["primary"]]
  fk <- tableOptions[[tablename]][["FK"]]
  replicates <- tableOptions[[tablename]]["replicates"]

  # Generate all the ids
  idUP <- paste0(glacierID,"_UP_")
  idDN <- paste0(glacierID,"_DN_")
  ids <- vector()
  idsFk <- vector()
  for (patch in 1:3) {

    for (replicate in replicates) {
      idsFk <- c(idsFk,paste0(idUP,patch))
      idsFk <- c(idsFk,paste0(idDN,patch))
      ids <- c(ids,paste0(idUP,patch,"_",replicate))
      ids <- c(ids,paste0(idDN,patch,"_",replicate))
    }
  }
  
  ids <- sort(ids)
  idsFk <- sort(idsFk)
  nbRow <- nrow(dataf)
  
  # Create a new empty dataframe
  newdataf <- dataf
  newdataf[,]=matrix(ncol=ncol(newdataf), rep(NA, prod(dim(newdataf))))
  
  # Add new rows to the df if necessary
  if(nbRow < length(ids))
  {
    newdataf <- addRows(newdataf,nbRow,length(ids))
  }

  # Fill the df with generated columns
  newdataf[[primary]] <- ids
  newdataf[[fk]] <- idsFk
  newdataf[["replicate"]] <- replicates
  
  newdataf <- copyDFValuesTo(dataf,newdataf,primary)
  
  return(newdataf)
}