

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
        selectInput(ns("location"),"Select a location", choices = c("Up"="UP","Down"="DN")),
        selectInput(ns("patch"),"Select a patch", choices = c(1,2,3)),
        actionButton(ns("generate"),"Generate template"),
      ),
      width = 4
    ),
    # Create the main panel with the innerModule first unit plot UI elements inside
    mainPanel(
      id = ns('main-upload'),
      div(
        rHandsontableOutput(ns("table")),
        actionButton(ns("upload"),"Upload data")
      ),
      width = 8
    )
  )
  
}


uploadDataTab <- function(input,output,session,pool){
  
  tableName <- reactive({input$type})
  
  dataf <- reactive({
    tmp <- getData(pool,isolate(tableName()),input$glacier)
    return(generateFilledDF(tmp,tableName(),input$glacier))
  })
  
  observeEvent(input$generate,{

    output$table <- renderRHandsontable({
      rhandsontable(dataf(),digits=10,stretchH = "all")%>% hot_cols(format = "0.000000000")
    })
  })
  
  observeEvent(input$upload,{
    out <- hot_to_r(input$table)
    print(out)
    saveData(out,tableName(),pool)
  })

  
}

getData <- function(pool,tableName,glacierID){
  conn <- poolCheckout(pool)
  queryStatus <- dbWithTransaction(conn,{
    query <- paste0("SELECT * FROM ",tableName," WHERE id_",tableName," LIKE '",glacierID,"%'")
    dataframe <-dbGetQuery(conn,query)
  })
  poolReturn(conn)
  return(dataframe)
}

saveData <- function(data,tableName,pool){
  
  request <- buildInsertQuery(data,tableName)
  
  #Send query to the database using pool
  check <- tryCatch({
    conn <- poolCheckout(pool)
    queryStatus <- dbWithTransaction(conn,{
      dbGetQuery(conn,request)
    })
    poolReturn(conn)
    print("Data successfully inserted into database")
    showNotification("Data successfully inserted into database",type = "message")
  },
  warning = function(war){
    print(war)
    showNotification(war$message, type = "warning")
  },
  error = function(err){
    print(err)
    showNotification(err$message,type = "error",duration = NULL)
  },
  finally = function(f){
    print(e) 
  })
  
}


# Build an insert sql query
# Params :
# - data : dataframe of data to insert into the database
# - tableName : name of the table in which the data will be inserted
# Return the built request in a string
buildInsertQuery <- function(data,tableName){
  request <- paste0(c('INSERT INTO ',tableName,' ('))
  headers <-colnames(data)
  for(x in headers){request <- paste0(c(request,"`",x,"`, "))}
  request <- paste(request,collapse = '')
  request <- substr(request,1,nchar(request)-2)
  request <- paste0(request,") VALUES ")
  for(row in 1:nrow(data)){
    request <- paste0(request,"(")
    for(value in data[row,])
    {
      if(is.na(value))
        request <- paste0(request,"NULL,")
      else
        request <- paste0(request,"'",value,"',")
    }
    request <- substr(request,1,nchar(request)-1)
    request <- paste0(request,"),")
  }
  request <- substr(request,1,nchar(request)-1)
  request <- paste0(request," AS new_values ON DUPLICATE KEY UPDATE ")
  for(x in headers[-1]){request <- paste0(request,x,"=new_values.",x,", ")}
  request <- substr(request,1,nchar(request)-2)
  print(request)
  return(request)
}


generateFilledDF <- function(dataf,tablename,glacierID){
  idUP <- paste0(glacierID,"_UP_")
  idDN <- paste0(glacierID,"_DN_")
  ids <- vector()
  idsFk <- vector()

  replicates <- c('A','B','C')
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
  print(ids)
  print(idsFk)
  nbOfRow <- length(ids)
  nbCol <- ncol(dataf)
  nbRow <- nrow(dataf)
  newdataf <- dataf

  newdataf[,]=matrix(ncol=ncol(newdataf), rep(NA, prod(dim(newdataf))))
  for (i in (nbRow+1):length(ids)) {
    row <- vector()
    for (j in 1:nbCol) {
      row <- c(row,NA)
    }
    newdataf <- rbind(newdataf,row)
  }
  newdataf[[paste0("id_",tablename)]] <- ids
  newdataf[[tableOptions[[tablename]][["FK"]]]] <- idsFk
  newdataf[[tableOptions[[tablename]][["name"]]]] <- replicates
  
  

    for (i in 1:nrow(dataf)) {
    row <-dataf[i,]
    id <- row[[paste0("id_",tablename)]]
    if(id %in% ids)
    {
        se <- newdataf[[paste0("id_",tablename)]]
        rownumber <- which(se == id,arr.ind = TRUE)
        newdataf[rownumber,] <- row
    }
  }
  
  print(newdataf)
  
  return(newdataf)
}